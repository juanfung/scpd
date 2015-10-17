/* create stata dictionary; see statadict.R */
/* import into stata*/

/* set directory as necessary */
cd "/home/juanfung/Data/ChicagoNY"

/* import fixed width assessment data */
infix using UIL_ASSR_LAYOUT.dct in 1 // "in 1" imports first row for testing

/* save as ... */
save "test1.dta"

/* import fixed width transactions data */    
infix using UIL_HIST_LAYOUT.dct in 1

/* save as ... */
save "test2.dta"
