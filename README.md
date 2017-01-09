# Youtube-dl Music Frontend

##About
This project is a simple frontend to youtube-dl so that it can intelligently find and download the proper youtube video for a song, rather than just stupidly finding the first result from a search. All that's required is a list of the songs you want.

##Installation
This program is written with Ada2012 and requires a Ada2012 GNAT compiler.

This project uses gnatGPR. As such, it will most likely be necessary to run 'gprconfig' and select your Ada compiler. Next, simply run 'gprbuild'. The program will be compiled. Now it can be run with one of the above example commands.

Finally, an up to date version of youtube-dl and curl are necessary. This is also linux only.

##How to
Create a file name consisting of artist name followed by song title. It is important that this is written how it would be on the record, and you will get better results by leaving out 'featuring' in the title. Notes the lack of space around the pipe.

	Artist Name|Their Song