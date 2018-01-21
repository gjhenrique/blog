#!/bin/sh

convert -loop 0 -delay 100 1-without_purpose-*.svg without-purpose.gif
convert -loop 0 -delay 100 2-without_configuration-*.svg without-configuration.gif
convert -loop 0 -delay 100 3-same_window_window_dedication-*.svg with-window-purpose.gif
convert -loop 0 -delay 100 4-same_window_buffer_dedication-*.svg  with-buffer-purpose.gif
convert -loop 0 -delay 100 5-two_frames_going_wrong-*.svg two-frames-problem.gif
convert -loop 0 -delay 100 6-two_frames_going_right-*.svg two-frames-okay.gif
convert -loop 0 -delay 150 7-three_frames-*.svg three-frames.gif

