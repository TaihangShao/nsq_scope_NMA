clear all
version 15
set more off

*PFS

* Set the working directory and import data
cd "C:/Users/ECHO/Desktop/nsq-cea-scope/extra check"
import delimited RP_PFS.csv, clear
*import delimited RP_OS.csv, clear
drop i

* set the colour scheme
set scheme s1color

* Declare survival data
stset time, fail(event=1)

*log-log plots
stphplot if studycode==1, by(txcode) legend(label(1 "che") label(2 "cam+che")) title("CameL")
graph export "C:\Users\ECHO\Desktop\nsq-cea-scope\plot\llhr-t\pfs\camel.png", as(png) replace
stphplot if studycode==2, by(txcode) legend(label(1 "che") label(2 "niv+ipi+che")) title("Checkmate9LA")
graph export "C:\Users\ECHO\Desktop\nsq-cea-scope\plot\llhr-t\pfs\cm9la.png", as(png) replace
stphplot if studycode==3, by(txcode) legend(label(1 "che") label(2 "sug")) title("Gemstone302")
graph export "C:\Users\ECHO\Desktop\nsq-cea-scope\plot\llhr-t\pfs\gs302.png", as(png) replace
stphplot if studycode==4, by(txcode) legend(label(1 "che") label(2 "ate+che")) title("Impower130")
graph export "C:\Users\ECHO\Desktop\nsq-cea-scope\plot\llhr-t\pfs\ip130.png", as(png) replace
stphplot if studycode==5, by(txcode) legend(label(1 "che") label(2 "ate+che")) title("Impower132")
graph export "C:\Users\ECHO\Desktop\nsq-cea-scope\plot\llhr-t\pfs\ip132.png", as(png) replace
stphplot if studycode==6, by(txcode) legend(label(1 "bev+che") label(2 "ate+bev+che") label(3 "ate+che")) title("Impower150")
graph export "C:\Users\ECHO\Desktop\nsq-cea-scope\plot\llhr-t\pfs\ip150.png", as(png) replace
stphplot if studycode==7, by(txcode) legend(label(1 "che") label(2 "pem+che")) title("Keynote189")
graph export "C:\Users\ECHO\Desktop\nsq-cea-scope\plot\llhr-t\pfs\kn189.png", as(png) replace
stphplot if studycode==8, by(txcode) legend(label(1 "che") label(2 "sin+che")) title("Orient11")
graph export "C:\Users\ECHO\Desktop\nsq-cea-scope\plot\llhr-t\pfs\o11.png", as(png) replace
stphplot if studycode==9, by(txcode) legend(label(1 "bev+che") label(2 "niv+bev+che")) title("TASUK2-52")
graph export "C:\Users\ECHO\Desktop\nsq-cea-scope\plot\llhr-t\pfs\t52.png", as(png) replace
stphplot if studycode==10, by(txcode) legend(label(1 "che") label(2 "tis+che")) title("Rationale304")
graph export "C:\Users\ECHO\Desktop\nsq-cea-scope\plot\llhr-t\pfs\r304.png", as(png) replace


*OS

cd "C:/Users/ECHO/Desktop/nsq-cea-scope/extra check"

import delimited RP_OS.csv, clear
drop i

* set the colour scheme
set scheme s1color

* Declare survival data
stset time, fail(event=1)

*log-log plots
stphplot if studycode==1, by(txcode) legend(label(1 "che") label(2 "cam+che")) title("CameL")
graph export "C:\Users\ECHO\Desktop\nsq-cea-scope\plot\llhr-t\os\camel.png", as(png) replace
stphplot if studycode==2, by(txcode) legend(label(1 "che") label(2 "niv+ipi")) title("Checkmate227")
graph export "C:\Users\ECHO\Desktop\nsq-cea-scope\plot\llhr-t\os\cm227.png", as(png) replace
stphplot if studycode==3, by(txcode) legend(label(1 "che") label(2 "niv+ipi+che")) title("Checkmate9LA")
graph export "C:\Users\ECHO\Desktop\nsq-cea-scope\plot\llhr-t\os\cm9la.png", as(png) replace
stphplot if studycode==4, by(txcode) legend(label(1 "che") label(2 "ate+che")) title("Impower130")
graph export "C:\Users\ECHO\Desktop\nsq-cea-scope\plot\llhr-t\os\ip130.png", as(png) replace
stphplot if studycode==5, by(txcode) legend(label(1 "che") label(2 "ate+che")) title("Impower132")
graph export "C:\Users\ECHO\Desktop\nsq-cea-scope\plot\llhr-t\os\ip132.png", as(png) replace
stphplot if studycode==6, by(txcode) legend(label(1 "bev+che") label(2 "ate+bev+che") label(3 "ate+che")) title("Impower150")
graph export "C:\Users\ECHO\Desktop\nsq-cea-scope\plot\llhr-t\os\ip150.png", as(png) replace
stphplot if studycode==7, by(txcode) legend(label(1 "che") label(2 "pem+che")) title("Keynote189")
graph export "C:\Users\ECHO\Desktop\nsq-cea-scope\plot\llhr-t\os\kn189.png", as(png) replace
stphplot if studycode==8, by(txcode) legend(label(1 "che") label(2 "sin+che")) title("Orient11")
graph export "C:\Users\ECHO\Desktop\nsq-cea-scope\plot\llhr-t\os\o11.png", as(png) replace
stphplot if studycode==9, by(txcode) legend(label(1 "bev+che") label(2 "niv+bev+che")) title("TASUK2-52")
graph export "C:\Users\ECHO\Desktop\nsq-cea-scope\plot\llhr-t\os\t52.png", as(png) replace


*OS tps small than 1

cd "C:/Users/ECHO/Desktop/nsq-cea-scope/extra check"

import delimited RP_tpss1_OS.csv, clear
drop i

* set the colour scheme
set scheme s1color

* Declare survival data
stset time, fail(event=1)

*log-log plots

stphplot if studycode==1, by(txcode) legend(label(1 "che") label(2 "niv+ipi+che")) title("Checkmate9LA")
graph export "C:\Users\ECHO\Desktop\nsq-cea-scope\plot\llhr-t\os-tps-s1\cm9la.png", as(png) replace
stphplot if studycode==2, by(txcode) legend(label(1 "che") label(2 "ate+che")) title("Impower132")
graph export "C:\Users\ECHO\Desktop\nsq-cea-scope\plot\llhr-t\os-tps-s1\ip132.png", as(png) replace
stphplot if studycode==3, by(txcode) legend(label(1 "bev+che") label(2 "ate+bev+che") label(3 "ate+che")) title("Impower150")
graph export "C:\Users\ECHO\Desktop\nsq-cea-scope\plot\llhr-t\os-tps-s1\ip150.png", as(png) replace
stphplot if studycode==4, by(txcode) legend(label(1 "che") label(2 "pem+che")) title("Keynote189")
graph export "C:\Users\ECHO\Desktop\nsq-cea-scope\plot\llhr-t\os-tps-s1\kn189.png", as(png) replace



*PFS tps small than 1

cd "C:/Users/ECHO/Desktop/nsq-cea-scope/extra check"

import delimited RP_tpss1_PFS.csv, clear
drop i

* set the colour scheme
set scheme s1color

* Declare survival data
stset time, fail(event=1)

*log-log plots

stphplot if studycode==1, by(txcode) legend(label(1 "che") label(2 "niv+ipi+che")) title("Checkmate9LA")
graph export "C:\Users\ECHO\Desktop\nsq-cea-scope\plot\llhr-t\pfs-tps-s1\cm9la.png", as(png) replace
stphplot if studycode==2, by(txcode) legend(label(1 "che") label(2 "ate+che")) title("Impower132")
graph export "C:\Users\ECHO\Desktop\nsq-cea-scope\plot\llhr-t\pfs-tps-s1\ip132.png", as(png) replace
stphplot if studycode==3, by(txcode) legend(label(1 "che") label(2 "pem+che")) title("Keynote189")
graph export "C:\Users\ECHO\Desktop\nsq-cea-scope\plot\llhr-t\pfs-tps-s1\kn189.png", as(png) replace
stphplot if studycode==4, by(txcode) legend(label(1 "che") label(2 "sin+che")) title("Orient11")
graph export "C:\Users\ECHO\Desktop\nsq-cea-scope\plot\llhr-t\pfs-tps-s1\o11.png", as(png) replace
stphplot if studycode==5, by(txcode) legend(label(1 "che") label(2 "tis+che")) title("Rationale304")
graph export "C:\Users\ECHO\Desktop\nsq-cea-scope\plot\llhr-t\pfs-tps-s1\r304.png", as(png) replace




*OS tps over than 50

cd "C:/Users/ECHO/Desktop/nsq-cea-scope/extra check"

import delimited RP_tpso50_OS.csv, clear
drop i

* set the colour scheme
set scheme s1color

* Declare survival data
stset time, fail(event=1)

*log-log plots

stphplot if studycode==1, by(txcode) legend(label(1 "che") label(2 "niv+ipi")) title("Checkmate227")
graph export "C:\Users\ECHO\Desktop\nsq-cea-scope\plot\llhr-t\os-tps-o50\cm227.png", as(png) replace
stphplot if studycode==2, by(txcode) legend(label(1 "che") label(2 "ate+che")) title("Impower132")
graph export "C:\Users\ECHO\Desktop\nsq-cea-scope\plot\llhr-t\os-tps-o50\ip132.png", as(png) replace
stphplot if studycode==3, by(txcode) legend(label(1 "bev+che") label(2 "ate+bev+che") label(3 "ate+che")) title("Impower150")
graph export "C:\Users\ECHO\Desktop\nsq-cea-scope\plot\llhr-t\os-tps-o50\ip150.png", as(png) replace
stphplot if studycode==4, by(txcode) legend(label(1 "che") label(2 "pem+che")) title("Keynote189")
graph export "C:\Users\ECHO\Desktop\nsq-cea-scope\plot\llhr-t\os-tps-o50\kn189.png", as(png) replace



*PFS tps over 50

cd "C:/Users/ECHO/Desktop/nsq-cea-scope/extra check"

import delimited RP_tpso50_PFS.csv, clear
drop i

* set the colour scheme
set scheme s1color

* Declare survival data
stset time, fail(event=1)

*log-log plots


stphplot if studycode==1, by(txcode) legend(label(1 "che") label(2 "ate+che")) title("Impower132")
graph export "C:\Users\ECHO\Desktop\nsq-cea-scope\plot\llhr-t\pfs-tps-o50\ip132.png", as(png) replace
stphplot if studycode==2, by(txcode) legend(label(1 "che") label(2 "pem+che")) title("Keynote189")
graph export "C:\Users\ECHO\Desktop\nsq-cea-scope\plot\llhr-t\pfs-tps-o50\kn189.png", as(png) replace
stphplot if studycode==3, by(txcode) legend(label(1 "che") label(2 "sin+che")) title("Orient11")
graph export "C:\Users\ECHO\Desktop\nsq-cea-scope\plot\llhr-t\pfs-tps-o50\o11.png", as(png) replace
stphplot if studycode==4, by(txcode) legend(label(1 "che") label(2 "tis+che")) title("Rationale304")
graph export "C:\Users\ECHO\Desktop\nsq-cea-scope\plot\llhr-t\pfs-tps-o50\r304.png", as(png) replace


