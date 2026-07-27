# zhGebiete 0.1.0

* Initial release of basic functions.

# zhGebiete 0.1.1

* Bugfix: If multiple results are found via the name search, the searched term 
is now shown in the selection panel.
* New feature: Selection of name searches, mus now be enabled by the option
`auswahl = TRUE`. The feature is implemented in all the wrapper functions
that have a name search included (i.e. `get_gemeinden()`, `get_regionen()`,
`get_raumplanungsregionen()`). This feature was implemented to allow the user
to get all names that e.g. end with "am Albis". This usecase might even be more
relevant than name harmonization.
