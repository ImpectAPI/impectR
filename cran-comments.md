## Submission

This is a minor release (2.5.6 -> 2.6.0) that adds new functions and output
columns. See NEWS.md for details.

## R CMD check results

0 errors | 0 warnings | 1 note

* checking for future file timestamps ... NOTE
  unable to verify current time

  This note is caused by the check environment being unable to reach the
  time server and is unrelated to the package.

## Test environments

* local macOS 26.7.1 (aarch64-apple-darwin20), R 4.4.3

## System requirements

The new function `getVideoClips()` requires the external 'ffmpeg' binary.
It is declared as optional under SystemRequirements in DESCRIPTION. The
function checks for 'ffmpeg' on the PATH and stops with an informative error
if it is not available. Its example is wrapped in \dontrun{} because it also
requires a valid API token, so no check needs 'ffmpeg'.

## Maintainer change

The maintainer's email address has changed from florian.schmitt@impect.com
to florian.schmitt@catapultsports.com due to a change in organizational
affiliation. The maintainer (Florian Schmitt) remains the same person.
An email confirming this change was sent to CRAN@r-project.org from the
previous address on 2026-08-10 prior to this submission.

The copyright holder "Impect GmbH" has been replaced by "Catapult Sports"
in Authors@R as part of this transition.
