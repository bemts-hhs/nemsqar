## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a minor version release from 1.0.0 to 1.1.0

I am still finding the following when I run devtools::check(manual = TRUE, cran = TRUE), not sure if it needs to be addressed.

ERROR: Unknown command "TMPDIR=C:/Users/nfoss0/AppData/Local/Temp/RtmpWcUuH3/file5b46d85848". Did you mean command "install"?
   Warning message:
   In system2("quarto", "-V", stdout = TRUE, env = paste0("TMPDIR=",  :
     running command '"quarto" TMPDIR=C:/Users/nfoss0/AppData/Local/Temp/RtmpWcUuH3/file5b46d85848 -V' had status 1
