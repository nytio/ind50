library("haven") # 2.5.1

# Descarga de microdatos desde:
# https://www.inegi.org.mx/programas/intercensal/2015/#Microdatos

tr <- read_sav("/media/mario/home/R/EIC2015/tr_persona01.sav")[,c(1:3,5,7,9,12:15)]
tr <- rbind(tr, read_sav("/media/mario/home/R/EIC2015/tr_persona02.sav")[,c(1:3,5,7,9,12:15)])
tr <- rbind(tr, read_sav("/media/mario/home/R/EIC2015/tr_persona03.sav")[,c(1:3,5,7,9,12:15)])
tr <- rbind(tr, read_sav("/media/mario/home/R/EIC2015/tr_persona04.sav")[,c(1:3,5,7,9,12:15)])
tr <- rbind(tr, read_sav("/media/mario/home/R/EIC2015/tr_persona05.sav")[,c(1:3,5,7,9,12:15)])
tr <- rbind(tr, read_sav("/media/mario/home/R/EIC2015/tr_persona06.sav")[,c(1:3,5,7,9,12:15)])
tr <- rbind(tr, read_sav("/media/mario/home/R/EIC2015/tr_persona07.sav")[,c(1:3,5,7,9,12:15)])
tr <- rbind(tr, read_sav("/media/mario/home/R/EIC2015/tr_persona08.sav")[,c(1:3,5,7,9,12:15)])
tr <- rbind(tr, read_sav("/media/mario/home/R/EIC2015/tr_persona09.sav")[,c(1:3,5,7,9,12:15)])
tr <- rbind(tr, read_sav("/media/mario/home/R/EIC2015/tr_persona10.sav")[,c(1:3,5,7,9,12:15)])
tr <- rbind(tr, read_sav("/media/mario/home/R/EIC2015/tr_persona11.sav")[,c(1:3,5,7,9,12:15)])
tr <- rbind(tr, read_sav("/media/mario/home/R/EIC2015/tr_persona12.sav")[,c(1:3,5,7,9,12:15)])
tr <- rbind(tr, read_sav("/media/mario/home/R/EIC2015/tr_persona13.sav")[,c(1:3,5,7,9,12:15)])
tr <- rbind(tr, read_sav("/media/mario/home/R/EIC2015/tr_persona14.sav")[,c(1:3,5,7,9,12:15)])
tr <- rbind(tr, read_sav("/media/mario/home/R/EIC2015/tr_persona15.sav")[,c(1:3,5,7,9,12:15)])
tr <- rbind(tr, read_sav("/media/mario/home/R/EIC2015/tr_persona16.sav")[,c(1:3,5,7,9,12:15)])
tr <- rbind(tr, read_sav("/media/mario/home/R/EIC2015/tr_persona17.sav")[,c(1:3,5,7,9,12:15)])
tr <- rbind(tr, read_sav("/media/mario/home/R/EIC2015/tr_persona18.sav")[,c(1:3,5,7,9,12:15)])
tr <- rbind(tr, read_sav("/media/mario/home/R/EIC2015/tr_persona19.sav")[,c(1:3,5,7,9,12:15)])
tr <- rbind(tr, read_sav("/media/mario/home/R/EIC2015/tr_persona20.sav")[,c(1:3,5,7,9,12:15)])
tr <- rbind(tr, read_sav("/media/mario/home/R/EIC2015/tr_persona21.sav")[,c(1:3,5,7,9,12:15)])
tr <- rbind(tr, read_sav("/media/mario/home/R/EIC2015/tr_persona22.sav")[,c(1:3,5,7,9,12:15)])
tr <- rbind(tr, read_sav("/media/mario/home/R/EIC2015/tr_persona23.sav")[,c(1:3,5,7,9,12:15)])
tr <- rbind(tr, read_sav("/media/mario/home/R/EIC2015/tr_persona24.sav")[,c(1:3,5,7,9,12:15)])
tr <- rbind(tr, read_sav("/media/mario/home/R/EIC2015/tr_persona25.sav")[,c(1:3,5,7,9,12:15)])
tr <- rbind(tr, read_sav("/media/mario/home/R/EIC2015/tr_persona26.sav")[,c(1:3,5,7,9,12:15)])
tr <- rbind(tr, read_sav("/media/mario/home/R/EIC2015/tr_persona27.sav")[,c(1:3,5,7,9,12:15)])
tr <- rbind(tr, read_sav("/media/mario/home/R/EIC2015/tr_persona28.sav")[,c(1:3,5,7,9,12:15)])
tr <- rbind(tr, read_sav("/media/mario/home/R/EIC2015/tr_persona29.sav")[,c(1:3,5,7,9,12:15)])
tr <- rbind(tr, read_sav("/media/mario/home/R/EIC2015/tr_persona30.sav")[,c(1:3,5,7,9,12:15)])
tr <- rbind(tr, read_sav("/media/mario/home/R/EIC2015/tr_persona31.sav")[,c(1:3,5,7,9,12:15)])
tr <- rbind(tr, read_sav("/media/mario/home/R/EIC2015/tr_persona32.sav")[,c(1:3,5,7,9,12:15)])

write_sav(tr, "/media/mario/home/R/EIC2015/tr_personaA.sav")

# GET FILE='/media/sf_Media/home/R/EIC2015/tr_personaA.sav'.
# DATASET NAME EncuestaIC2015 WINDOW=FRONT.
# 
# WEIGHT BY FACTOR.
# DATASET ACTIVATE EncuestaIC2015.
# SAVE OUTFILE='/media/sf_Media/home/R/EIC2015/tr_personaA.sav'
# /COMPRESSED.
# 
# CROSSTABS
# /TABLES=EDAD BY SEXO
# /FORMAT=AVALUE TABLES
# /CELLS=COUNT
# /COUNT ROUND CELL.
