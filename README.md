# atc

Scrapper to download all ATC data from [WHOCC](https://www.whocc.no/atc_ddd_index/)

This is not the only code to do this work. The noteable difference between the code provided here and other code examples is that this as been written in "off-the-shelf" R. No additional packages are requried. If you have R installed you should be able to run the script with no additional installs.

The code will download the ATC data and write a .csv which can then be imported into to other projects as needed.

It forked from https://github.com/dewittpe/atc.git. The original stopped on error in parsing H03CA code. I am not a programmer of R. Therefore I added code to make it continue.
https://www.whocc.no/atc_ddd_index/?code=H03CA&showdescription=no

"H03CA","Iodine therapy",NA,NA,NA,NA
"","","0.33","g","O",NA
"","","0.33","g","P",NA
