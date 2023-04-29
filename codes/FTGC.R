# Forest type group function

# take forest type code from FIA and group

FTGC <- function(fortype){ # fortype should be vector of forest type codes
  ftg <- ifelse(fortype < 120, 100,
                ifelse(fortype < 140, 120,
                       ifelse(fortype < 150, 140,
                              ifelse(fortype < 160, 150,
                                     ifelse(fortype<170, 160,
                                            ifelse(fortype<180, 170,
                                                   ifelse(fortype<200, 180,
                                                          ifelse(fortype<220, 200,
                                                                 ifelse(fortype<240, 220,
                                                                        ifelse(fortype<260, 240,
                                                                               ifelse(fortype<280, 260,
                                                                                      ifelse(fortype<300,280,
                                                                                             ifelse(fortype<320, 300,
                                                                                                    ifelse(fortype<340, 320,
                                                                                                           ifelse(fortype<360, 340,
                                                                                                                  ifelse(fortype<370, 360,
                                                                                                                         ifelse(fortype<380, 370,
                                                                                                                                ifelse(fortype<390, 380,
                                                                                                                                       ifelse(fortype<400, 390,
                                                                                                                                              ifelse(fortype<500, 400,
                                                                                                                                                     ifelse(fortype<600, 500,
                                                                                                                                                            ifelse(fortype<700,600,
                                                                                                                                                                   ifelse(fortype<800,700,
                                                                                                                                                                          ifelse(fortype<900,800,
                                                                                                                                                                                 ifelse(fortype<910, 900,
                                                                                                                                                                                        ifelse(fortype<920, 910,
                                                                                                                                                                                               ifelse(fortype<940,920,
                                                                                                                                                                                                      ifelse(fortype<960, 940,
                                                                                                                                                                                                             ifelse(fortype<970,960,
                                                                                                                                                                                                                    ifelse(fortype<980, 970,
                                                                                                                                                                                                                           ifelse(fortype<988, 980,
                                                                                                                                                                                                                                  ifelse(fortype<990,988,
                                                                                                                                                                                                                                         ifelse(fortype<999,990,999)))))))))))))))))))))))))))))))))
  return(ftg)
}
