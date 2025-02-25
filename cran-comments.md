## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a new release.

I found the following message when running `devtools::check()`

─  checking tests ...
    [38s] OKtestthat.R'
   * checking for non-standard things in the check directory ... OK
   * checking for detritus in the temp directory ... OK
   * DONE
   
   Status: OK
   
   ERROR: Unknown command "TMPDIR=C:/Users/nfoss0/AppData/Local/Temp/Rtmpg3jWnB/file2dac60921739". Did you mean command "install"?
   Warning message:
   In system2("quarto", "-V", stdout = TRUE, env = paste0("TMPDIR=",  :
     running command '"quarto" TMPDIR=C:/Users/nfoss0/AppData/Local/Temp/Rtmpg3jWnB/file2dac60921739 -V' had status 1
     
I am not sure how to address this.  Any insight into this issue would be helpful.
