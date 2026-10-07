#' Cut a video clip around each event of an events dataframe and merge them
#' into a single video file
#'
#' For every row of the (pre-filtered) \code{events} dataframe a clip window is
#' built as \code{gameTimeInSec - lead} to \code{gameTimeInSec + duration + lag},
#' the corresponding match video is fetched from the 'IMPECT' API and cut with
#' 'ffmpeg', and all clips are merged into a single file at
#' \code{output_path}. Input row order is preserved and every clip is
#' normalized to a common resolution and frame rate before merging.
#'
#' Events whose match video is not available to the user (HTTP 403) are
#' skipped with one warning per match. Clips that contain no footage or fail
#' for any other reason (e.g. API or 'ffmpeg' errors) are skipped with a
#' warning. An error is only raised if the token is invalid or none of the
#' requested clips could be created.
#'
#' This is an interim solution: the API currently serves full match videos
#' that have to be cut client-side. It will most likely be replaced by an API
#' endpoint that returns already-cut clips.
#'
#' Requires the 'ffmpeg' binary to be installed and available on the
#' \code{PATH} (see \url{https://ffmpeg.org/download.html}).
#'
#' @param events dataframe of events, e.g. a filtered output of
#' \code{getEvents()}, containing the columns \code{matchId},
#' \code{gameTimeInSec} and \code{duration}
#' @param output_path path of the merged output video file, must end in
#' \code{.mp4}
#' @param token bearer token
#' @param lead seconds to include before each event
#' @param lag seconds to include after each event
#' @param target_width width of the output video in pixels
#' @param target_height height of the output video in pixels
#' @param target_fps frame rate of the output video
#' @param warn_above_seconds warn if the expected total output length exceeds
#' this number of seconds
#' @param host host environment
#'
#' @export
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @return the path of the created video file
#'
#' @examples
#' # Real usage: requires valid Bearer Token from `getAccessToken()` and
#' # 'ffmpeg' on the PATH
#' \dontrun{
#' events <- getEvents(matches = c(84248), token = "yourToken")
#' shots <- events[events$actionType == "SHOT", ]
#' getVideoClips(
#'   events = shots,
#'   output_path = "shots.mp4",
#'   token = "yourToken"
#' )
#' }
getVideoClips <- function (
    events,
    output_path,
    token,
    lead = 3,
    lag = 3,
    target_width = 1920,
    target_height = 1080,
    target_fps = 25,
    warn_above_seconds = 600,
    host = "https://api.impect.com"
) {

  # warn that this is an interim, client-side solution
  base::warning(
    "getVideoClips() cuts full match videos client-side using ffmpeg. This is ",
    "an interim solution and will most likely be replaced by an API endpoint ",
    "that returns already-cut clips.",
    call. = FALSE
  )

  # ensure ffmpeg is available before doing any work
  if (base::Sys.which("ffmpeg") == "") {
    base::stop(
      "ffmpeg was not found on PATH. getVideoClips() requires ffmpeg to be ",
      "installed (see https://ffmpeg.org/download.html).",
      call. = FALSE
    )
  }

  # validate input dataframe
  if (!base::is.data.frame(events) || base::nrow(events) == 0) {
    base::stop("The provided events dataframe is empty.", call. = FALSE)
  }

  required_columns <- base::c("matchId", "gameTimeInSec", "duration")
  missing_columns <- base::setdiff(required_columns, base::names(events))
  if (base::length(missing_columns) > 0) {
    base::stop(
      "The provided events dataframe is missing required column(s): ",
      base::paste(missing_columns, collapse = ", "), ".",
      call. = FALSE
    )
  }

  # validate output path; every clip is encoded as H.264/AAC in an MP4
  # container, so the output has to be an .mp4 file
  if (!base::is.character(output_path) || base::length(output_path) != 1 ||
      !base::grepl("\\.mp4$", output_path, ignore.case = TRUE)) {
    base::stop(
      "'output_path' must be a single file path ending in '.mp4'.",
      call. = FALSE
    )
  }

  # validate lead and lag
  for (arg in base::c("lead", "lag")) {
    value <- base::get(arg)
    if (!base::is.numeric(value) || base::length(value) != 1 ||
        !base::is.finite(value) || value < 0) {
      base::stop(
        "'", arg, "' must be a single non-negative number.",
        call. = FALSE
      )
    }
  }

  # validate column types
  for (column in base::c("gameTimeInSec", "duration")) {
    if (!base::is.numeric(events[[column]])) {
      base::stop(
        "Column '", column, "' of the provided events dataframe must be ",
        "numeric.",
        call. = FALSE
      )
    }
  }

  # validate that no required value is missing
  invalid_rows <- base::which(
    base::is.na(events$matchId) |
      !base::is.finite(events$gameTimeInSec) |
      !base::is.finite(events$duration)
  )
  if (base::length(invalid_rows) > 0) {
    base::stop(
      "The provided events dataframe contains missing values in 'matchId', ",
      "'gameTimeInSec' or 'duration' in row(s): ",
      base::paste(
        invalid_rows[base::seq_len(base::min(20, base::length(invalid_rows)))],
        collapse = ", "
      ),
      if (base::length(invalid_rows) > 20) ", ..." else "",
      ". Please remove these rows before calling getVideoClips().",
      call. = FALSE
    )
  }

  # build clip windows, preserving the input row order
  clips <- events %>%
    dplyr::select(dplyr::all_of(required_columns)) %>%
    dplyr::mutate(
      startTime = base::pmax(0, .data$gameTimeInSec - lead),
      endTime = .data$gameTimeInSec + .data$duration + lag
    )

  # warn (do not block) if the expected total output length is large, as
  # fetching and re-encoding scales with the total number of seconds of video
  expected_seconds <- base::sum(clips$duration + lead + lag)
  if (expected_seconds > warn_above_seconds) {
    base::warning(
      base::sprintf(
        paste0(
          "The requested clips add up to ~%.1f minutes of video across %d ",
          "event(s). Fetching and re-encoding may be slow and produce a large ",
          "file. Raise 'warn_above_seconds' to silence this warning."
        ),
        expected_seconds / 60, base::nrow(clips)
      ),
      call. = FALSE
    )
  }

  # ensure the output directory exists
  base::dir.create(
    base::dirname(output_path), recursive = TRUE, showWarnings = FALSE
  )

  # build the ffmpeg filter that normalizes every clip to a common output spec
  # so the clips can be concatenated with a cheap stream copy afterwards
  normalize_filter <- base::sprintf(
    paste0(
      "scale=%d:%d:force_original_aspect_ratio=decrease,",
      "pad=%d:%d:(ow-iw)/2:(oh-ih)/2,setsar=1,fps=%d"
    ),
    target_width, target_height, target_width, target_height, target_fps
  )

  # cut every clip into its own file inside a temporary directory
  tmp_dir <- base::tempfile("impectR_clips_")
  base::dir.create(tmp_dir)
  base::on.exit(base::unlink(tmp_dir, recursive = TRUE), add = TRUE)

  clip_files <- base::c()
  skipped <- 0
  forbidden_matches <- base::c()

  for (i in base::seq_len(base::nrow(clips))) {
    clip <- clips[i, ]
    match_id <- clip$matchId

    # once a match is known to be forbidden, skip its remaining clips without
    # re-requesting or warning again
    if (match_id %in% forbidden_matches) {
      skipped <- skipped + 1
      next
    }

    # fetch and cut this clip; any failure only skips this clip so that the
    # clips that were already cut are not lost
    result <- base::tryCatch(
      .cutClip(clip, i, host, token, tmp_dir, normalize_filter),
      error = function(e) {
        # an invalid token affects every clip -> abort instead of skipping
        if (base::grepl("status code 401", base::conditionMessage(e),
                        fixed = TRUE)) {
          base::stop(e)
        }
        base::warning(
          base::sprintf(
            "Could not create the clip for match %s, row %d (%s); skipping it.",
            match_id, i, base::conditionMessage(e)
          ),
          call. = FALSE
        )
        NULL
      }
    )

    if (base::identical(result, "forbidden")) {
      # the user has no access to this match's video -> warn once and skip all
      # of its clips instead of aborting the whole reel
      base::warning(
        base::sprintf(
          paste0(
            "The video for match %s is not available to this user (HTTP 403); ",
            "skipping all clips from this match."
          ),
          match_id
        ),
        call. = FALSE
      )
      forbidden_matches <- base::c(forbidden_matches, match_id)
      skipped <- skipped + 1
      next
    }

    # failed or empty clips have already been warned about
    if (base::is.null(result)) {
      skipped <- skipped + 1
      next
    }

    clip_files <- base::c(clip_files, result)
  }

  # raise if no clip could be created
  if (base::length(clip_files) == 0) {
    base::stop(
      base::sprintf(
        paste0(
          "No video clips could be created: all %d requested clip(s) were ",
          "unavailable to this user, contained no footage or failed."
        ),
        base::nrow(clips)
      ),
      call. = FALSE
    )
  }

  # merge the clips inside the temporary directory first, so an existing file
  # at output_path is only replaced once the merge has succeeded
  if (base::length(clip_files) == 1) {
    # single clip: nothing to merge
    merged_file <- clip_files[1]
  } else {
    # multiple clips share one format -> stream copy is safe
    merged_file <- base::file.path(tmp_dir, "merged.mp4")
    # all clips live next to the concat list, which resolves relative paths
    # against its own directory -> list bare file names so special characters
    # in the temp dir path (e.g. quotes) never reach the concat parser
    concat_file <- base::file.path(tmp_dir, "clips.txt")
    base::writeLines(
      base::sprintf("file '%s'", base::basename(clip_files)),
      concat_file
    )
    .runFfmpeg(
      base::c(
        "-f", "concat", "-safe", "0", "-i", concat_file,
        "-c", "copy", merged_file
      )
    )
  }

  # move the merged file to its final destination
  if (!base::file.copy(merged_file, output_path, overwrite = TRUE)) {
    base::stop(
      base::sprintf(
        paste0(
          "Could not write the video file to '%s'. Please check that the path ",
          "is writable."
        ),
        output_path
      ),
      call. = FALSE
    )
  }

  if (skipped > 0) {
    base::message(
      base::sprintf(
        "Created %s from %d clip(s); skipped %d unavailable or failed clip(s)",
        output_path, base::length(clip_files), skipped
      )
    )
  } else {
    base::message(
      base::sprintf(
        "Created %s from %d clip(s)", output_path, base::length(clip_files)
      )
    )
  }

  # return path of the created file
  return(output_path)
}


#' Fetch the video for a single clip window and cut it into a local file
#'
#' @noRd
#'
#' @param clip single row of the clip windows dataframe
#' @param i row index of the clip, used for file names and messages
#' @param host host environment
#' @param token bearer token
#' @param tmp_dir directory to write the clip file to
#' @param normalize_filter ffmpeg video filter normalizing the output format
#'
#' @return the path of the clip file, "forbidden" if the user has no access to
#' the match video, or NULL if the clip window contains no footage
.cutClip <- function (clip, i, host, token, tmp_dir, normalize_filter) {

  match_id <- clip$matchId

  # get the video url and the actual video timestamps for this clip's match
  response <- .callAPIlimited(
    host,
    base_url = "/v5/customerapi/matches/",
    id = match_id,
    suffix = base::paste0(
      "/videos?start=", .formatSeconds(clip$startTime),
      "&end=", .formatSeconds(clip$endTime)
    ),
    token = token,
    ignore_403 = TRUE
  )

  if (httr::status_code(response) == 403) {
    return("forbidden")
  }

  video <- jsonlite::fromJSON(
    httr::content(response, "text", encoding = "UTF-8"),
    simplifyVector = FALSE
  )$data

  # the endpoint may return a single object or a list of objects
  if (base::is.null(base::names(video)) && base::length(video) > 0) {
    video <- video[[1]]
  }

  if (base::length(video) == 0) {
    base::stop(
      base::sprintf(
        "No video returned for match %s between %ss and %ss.",
        match_id, clip$startTime, clip$endTime
      ),
      call. = FALSE
    )
  }

  # extract data
  video_url <- video$url
  video_start_time <- video$timestamps$start$videoTimeInSec
  video_end_time <- video$timestamps$end$videoTimeInSec

  if (base::is.null(video_url) || base::is.null(video_start_time) ||
      base::is.null(video_end_time)) {
    base::stop(
      base::sprintf(
        paste0(
          "Unexpected response from the videos endpoint for match %s: ",
          "missing 'url' or 'timestamps' (fields returned: %s)."
        ),
        match_id,
        base::paste(base::names(base::unlist(video)), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  clip_file <- base::file.path(tmp_dir, base::sprintf("clip_%d.mp4", i))

  # re-encode + normalize so clips from different videos share one format;
  # keep exactly one video and at most one audio stream
  .runFfmpeg(
    base::c(
      "-ss", .formatSeconds(video_start_time),
      "-to", .formatSeconds(video_end_time),
      "-i", video_url,
      "-map", "0:v:0", "-map", "0:a:0?",
      "-vf", normalize_filter,
      "-c:v", "libx264", "-preset", "veryfast", "-crf", "20",
      "-pix_fmt", "yuv420p",
      "-c:a", "aac", "-ar", "48000", "-ac", "2",
      clip_file
    )
  )

  # ffmpeg exits successfully but writes a file without streams if the
  # requested window contains no footage -> warn and skip this clip
  if (!.hasStream(clip_file, "Video")) {
    base::warning(
      base::sprintf(
        paste0(
          "The video for match %s contains no footage for row %d (game time ",
          "%ss to %ss, video time %ss to %ss); skipping this clip."
        ),
        match_id, i, .formatSeconds(clip$startTime),
        .formatSeconds(clip$endTime), .formatSeconds(video_start_time),
        .formatSeconds(video_end_time)
      ),
      call. = FALSE
    )
    return(NULL)
  }

  # the concat stream copy requires every clip to have the same stream
  # layout, so add a silent audio track to clips whose source had no audio
  if (!.hasStream(clip_file, "Audio")) {
    silent_file <- base::file.path(
      tmp_dir, base::sprintf("clip_%d_silent.mp4", i)
    )
    .runFfmpeg(
      base::c(
        "-i", clip_file,
        "-f", "lavfi", "-i", "anullsrc=r=48000:cl=stereo",
        "-map", "0:v:0", "-map", "1:a:0",
        "-c:v", "copy",
        "-c:a", "aac", "-ar", "48000", "-ac", "2",
        "-shortest",
        silent_file
      )
    )
    clip_file <- silent_file
  }

  clip_file
}


#' Run ffmpeg quietly and raise an error containing ffmpeg's output on failure
#'
#' @noRd
#'
#' @param args character vector of ffmpeg arguments
.runFfmpeg <- function (args) {

  # run ffmpeg and capture its output
  output <- base::suppressWarnings(
    base::system2(
      "ffmpeg",
      base::shQuote(
        base::c("-hide_banner", "-loglevel", "error", "-nostats", "-y", args)
      ),
      stdout = TRUE,
      stderr = TRUE
    )
  )

  # surface ffmpeg's own error output to help debugging
  status <- base::attr(output, "status")
  if (!base::is.null(status) && status != 0) {
    base::stop(
      "ffmpeg failed:\n", base::paste(output, collapse = "\n"),
      call. = FALSE
    )
  }

  invisible(NULL)
}


#' Check whether a local video file contains a stream of the given type
#'
#' @noRd
#'
#' @param file path of the video file
#' @param type stream type as printed by ffmpeg, e.g. "Video" or "Audio"
#'
#' @return TRUE if the file contains at least one stream of the given type
.hasStream <- function (file, type) {

  # without an output file ffmpeg prints the stream info and exits non-zero
  output <- base::suppressWarnings(
    base::system2(
      "ffmpeg",
      base::shQuote(base::c("-hide_banner", "-i", file)),
      stdout = TRUE,
      stderr = TRUE
    )
  )

  base::any(
    base::grepl(base::paste0("Stream #[0-9]+:[0-9]+.*: ", type, ":"), output)
  )
}


#' Format a number of seconds without scientific notation
#'
#' @noRd
#'
#' @param x numeric value
.formatSeconds <- function (x) {
  base::format(x, scientific = FALSE, trim = TRUE, digits = 15)
}
