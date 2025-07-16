## R CMD check results

0 errors | 0 warnings | 1 note

* This is a patch release. 
* No breaking changes.
* Patch addresses the following:

* In airway_01_population(), the filter_process object had one text descriptor "All initial population successful intubation with no hypoxia or hypoxia/hypotension"
which was corrected to be "All initial population successful intubation with no hypoxia/hypotension".

* Also, in trauma_14_population(), the filter_process object has one text descriptor "Tournique procedure" which was corrected to be "Tourniquet procedure".

* Additionally, trauma_14_population() should *just work* after passing an arbitrary number of (applicable) columns to the transport_disposition argument. In {nemsqar} 1.1.0, only one column will work as the `grepl()` call is not wrapped in `dpyr::if_any()`. This was fixed so that users can reference other columns that contain transport disposition data, such as eDisposition.12 from NEMSIS 3.4 (or earlier versions as applicable). This helps with back compatibility when using this measure to earlier versions of NEMSIS, or later versions.

*** I found the following NOTE ***

❯ checking for future file timestamps ... NOTE
  unable to verify current time
