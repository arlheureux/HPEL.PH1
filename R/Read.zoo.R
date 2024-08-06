## _________________ Infos gÃ©nÃ©rales
## Nom : Read.zoo.R
## Auteur : Arnaud Lheureux
## Email: arnaud.lheureux@sorbonne-universite.fr
##
## Date de crÃ©ation : 25 juin 2024
##
## Copyright (c) Arnaud Lheureux, 2024
## R version : 4.4
## _________________


## _________________ Notes
#
#' Fonction qui groupes les tableaux
#' @param data_rephy DonnÃ©es rephy
#' @param data_phytobs DonnÃ©es phytobs
#' @param CodeTaxon Tableau de regroupement des UT
#' @param TF Base fonctionnelle
#' @return Les donnÃ©es phyto ok
#
## ---------------------------

Read.zoo <- function(
    #data_dyfamed, data_point_b, 
    data_ecotaxa,
    CodesTaxons,
    TF) {

#     ## Dyfamed
#     # importer en utf-8
#     data <- do.call("rbind", pblapply(
#         data_dyfamed, read.table, fileEncoding = "utf-8",
#         sep = "\t", header = TRUE
#     ))

    CodeTaxon <- read.table(CodesTaxons, sep = ";", header = TRUE, fileEncoding = "utf-8")
    TF <- as.data.frame(readxl::read_xlsx(path = "Masterlist-V5_final-July_2022.xlsx", sheet = 1))
#     summary(data)
#     head(CodeTaxon)

#     data <- data[grep("not-living", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("egg", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("gonophore", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("nectophore", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("bract", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("eudoxie", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("Bacteria", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("other", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("temporary", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("Harosa", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("nauplii", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("larvae", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("ephyra", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("part", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("juvenile", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("endostyle", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("zoea", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("head", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("tail", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("siphonula", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("trunk", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("nucleus", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("cyphonaute", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("calyptopsis", data$object_annotation_hierarch, invert = TRUE), ]



#    # sort(unique(data[, "object_annotation_category"]))

#     data[, "object_annotation_category"] <- gsub(".*<", "", data[, "object_annotation_category"])


#     data$Param <- CodeTaxon[match(data$object_annotation_category, CodeTaxon$Taxons),"Code"]

#     #unique(data$object_annotation_hierarchy)
#     head(data)
#     unique(data$object_date)
#     sort(table(data$object_annotation_category))

#     d2 <- data.frame(
#         Site = "Dyfamed",
#         Date = data$object_date,
#         Latitude = data$object_lat,
#         Longitude = data$object_lon,
#         Param = data$Param,
#         UT = data$Param,
#         Depth.min = data$object_depth_min,
#         Depth.max = data$object_depth_max
#     )
#     d2$Val <- (1 * data$acq_sub_part) / data$sample_tot_vol
#     d2$Size <- data$object_major * data$process_particle_pixel_size_mm
#     d2[d2$Size >= 2, "Size.id"] <- "Large"
#     d2[d2$Size < 2, "Size.id"] <- "Small"
#     d2$Year <- year(d2$Date)
#     d2$Month <- month(d2$Date)
#     d2$YearTS <- decimal_date(as.Date(d2$Date))

#     d3 <- do.call(
#         "rbind",
#         lapply(
#             unique(d2$Date),
#             function(d) {
#                 tmp <- d2[d2$Date == d, ]

#                 out01 <- do.call(
#                     "rbind",
#                     lapply(
#                         unique(tmp$Param),
#                         function(p) {
#                             tmp2 <- tmp[tmp$Param == p, ]
#                             tmp3 <- tmp2[1, ]
#                             tmp3$Val <- sum(tmp2$Val, na.rm = TRUE)
#                             return(tmp3)
#                         }
#                     )
#                 )
#                 return(out01)
#             }
#         )
#     )
#     head(d3)

#     # Je ne garde que ceux > 0.6 (dans un premier temps), les autres je dÃ©grade le niv taxo
#     sort(table(d3$Param)) / uniqueN(d3$Date)

#     # Par groupe de taille
#     d4 <- do.call(
#         "rbind",
#         lapply(
#             unique(d2$Date),
#             function(d) {
#                 tmp <- d2[d2$Date == d, ]

#                 out01 <- do.call(
#                     "rbind",
#                     lapply(
#                         unique(tmp$Size.id),
#                         function(p) {
#                             tmp2 <- tmp[tmp$Size.id == p, ]
#                             tmp3 <- tmp2[1, ]
#                             tmp3$Size = NA
#                             tmp3$Param = p
#                             tmp3$Val <- sum(tmp2$Val, na.rm = TRUE)
#                             return(tmp3)
#                         }
#                     )
#                 )
#                 return(out01)
#             }
#         )
#     )

#     head(d4)


#     ## PointB
#     # importer en utf-8
#     colnames_base <- colnames(read.table(data_point_b[1],
#         fileEncoding = "utf-8",
#         sep = "\t", header = TRUE
#     ))

#     #Attention date a prendre que des dates correspondantes entre les tailles de filets


#     c <- lapply(data_point_b[17:18], function(x) {
#         colnames(read.table(x,
#             fileEncoding = "utf-8",
#             sep = "\t", header = TRUE
#         ))
#     })
#     c
#     lapply(c, length)
#     !c[[1]] %in% c[[2]]
#     c[[1]][130]
#     c[[1]]

#     "process_particle_pixel_size_Âµm"



#     # Ici je dois rentrer dans les fichiers pour virer la colonne "process_particle_pixel_size_Âµm"
#     # qui est prÃ©sente que dans 1 fichier...
#     data <- do.call(
#         "rbind", 
#         pbmclapply(
#             data_point_b,
#             function(x) { 
#                 tmp <- read.table(x,
#                     fileEncoding = "utf-8",
#                     sep = "\t", header = TRUE
#                 )

#                 tmp <- tmp[, colnames(tmp) != "process_particle_pixel_size_Âµm"]
#                 return(tmp)
#             }
#     ))
#     summary(data)

#     data <- data[grep("not-living", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("egg", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("gonophore", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("nectophore", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("bract", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("eudoxie", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("Bacteria", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("other", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("temporary", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("Harosa", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("nauplii", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("larvae", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("ephyra", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("part", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("juvenile", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("endostyle", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("zoea", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("head", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("tail", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("siphonula", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("trunk", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("nucleus", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("cyphonaute", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("calyptopsis", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("actinula", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("like", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("megalopa", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("pluteus", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("phyllosoma", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("scale", data$object_annotation_hierarch, invert = TRUE), ]
#     data <- data[grep("wing", data$object_annotation_hierarch, invert = TRUE), ]


#    # sort(unique(data[, "object_annotation_category"]))

#    data[, "object_annotation_category"] <- gsub(".*<", "", data[, "object_annotation_category"])


#    data$Param <- CodeTaxon[match(data$object_annotation_category, CodeTaxon$Taxons), "Code"]
#     #data$Param <- data$object_annotation_category

#   # unique(data$object_annotation_hierarchy)
#   head(data)
#   unique(data$object_date)
#   sort(table(data$object_annotation_category))

#   d20 <- data.frame(
#       Site = "Point B",
#       Date = paste(str_sub(data$object_date, 1, 4), str_sub(data$object_date, 5, 6), str_sub(data$object_date, 7, 8),sep = "-"),
#       Latitude = data$object_lat,
#       Longitude = data$object_lon,
#       Param = data$Param,
#       UT = data$Param,
#       Depth.min = data$object_depth_min,
#       Depth.max = data$object_depth_max
#   )
  
#   d20$Val <- (1 * data$acq_sub_part) / data$sample_tot_vol
#   d20$Size <- data$object_major * data$process_particle_pixel_size_mm
#   d20[d20$Size >= 2, "Size.id"] <- "Large"
#   d20[d20$Size < 2, "Size.id"] <- "Small"
#   d20$Year <- year(d20$Date)
#   d20$Month <- month(d20$Date)
#   d20$YearTS <- decimal_date(as.Date(d20$Date))

#   d30 <- do.call(
#       "rbind",
#       lapply(
#           unique(d20$Date),
#           function(d) {
#               tmp <- d20[d20$Date == d, ]

#               out01 <- do.call(
#                   "rbind",
#                   lapply(
#                       unique(tmp$Param),
#                       function(p) {
#                           tmp2 <- tmp[tmp$Param == p, ]
#                           tmp3 <- tmp2[1, ]
#                           tmp3$Val <- sum(tmp2$Val, na.rm = TRUE)
#                           return(tmp3)
#                       }
#                   )
#               )
#               return(out01)
#           }
#       )
#   )
#   head(d30)

#   # Je ne garde que ceux > 0.6 (dans un premier temps), les autres je dÃ©grade le niv taxo
#   sort(table(d30$Param)) / uniqueN(d30$Date)

#   # Par groupe de taille
#   d40 <- do.call(
#       "rbind",
#       lapply(
#           unique(d20$Date),
#           function(d) {
#               tmp <- d20[d20$Date == d, ]

#               out01 <- do.call(
#                   "rbind",
#                   lapply(
#                       unique(tmp$Size.id),
#                       function(p) {
#                           tmp2 <- tmp[tmp$Size.id == p, ]
#                           tmp3 <- tmp2[1, ]
#                           tmp3$Size <- NA
#                           tmp3$Param <- p
#                           tmp3$Val <- sum(tmp2$Val, na.rm = TRUE)
#                           return(tmp3)
#                       }
#                   )
#               )
#               return(out01)
#           }
#       )
#   )

#   head(d40)

#     return(
#         list(
#             UT = d3,
#             Taille = d4
#         )
#     )



    data <- do.call(
        "rbind",
        lapply(
            eco,
            function(x) {
                event <- read.table(
                    file = grep("event", x, value = TRUE),
                    fileEncoding = "utf-8", sep = "\t", header = TRUE
                )
                ext_measure <- read.table(
                    file = grep("extended", x, value = TRUE),
                    fileEncoding = "utf-8", sep = "\t", header = TRUE
                )
                occ <- read.table(
                    file = grep("occurrence", x, value = TRUE),
                    fileEncoding = "utf-8", sep = "\t", header = TRUE
                )

                i <- unique(event$id)[1]
                data.out <- do.call(
                    "rbind",
                    mclapply(
                        unique(event$id),
                        function(i) {
                            out1 <- event[event$id == i, ]
                            out2 <- occ[occ$id == i, ]
                            out3 <- ext_measure[ext_measure$id == i, ]

                            o <- unique(out2$occurrenceID)[3]
                            out4 <- do.call(
                                "rbind",
                                lapply(
                                    unique(out2$occurrenceID),
                                    function(o) {
                                        if (!o %in% out3$occurrenceID) {
                                            return()
                                        } else {
                                            tmp2 <- out2[out2$occurrenceID == o, ]
                                            tmp3 <- out3[out3$occurrenceID == o, ]

                                            tmp2$aphiaID <- gsub(".*:", "", tmp2$scientificNameID)
                                            tmp2$Abondance <- as.numeric(tmp3[grep("Abundance", tmp3$measurementType), "measurementValue"])
                                            tmp2$Biovolume <- as.numeric(tmp3[grep("Biovolume", tmp3$measurementType), "measurementValue"])
                                            tmp2 <- tmp2[, colnames(tmp2) %in% c("id", "eventID", "occurrenceID", "scientificName", "aphiaID", "Abondance", "Biovolume")]
                                            return(tmp2)
                                        }
                                    }
                                )
                            )

                            Site <- ifelse(
                                grepl("Point B", out1$datasetName),
                                "Point B",
                                ifelse(
                                    grepl("DYFAMED", out1$datasetName),
                                    "Dyfamed",
                                    NA
                                )
                            )

                            out4$Site <- Site
                            out4$Longitude <- out1$decimalLongitude
                            out4$Latitude <- out1$decimalLatitude
                            out4$Date <- as.Date(out1$eventDate)
                            out4$YearTS <- round(decimal_date(out4$Date), 3)
                            out4$Year <- year(out4$Date)
                            out4$Month <- month(out4$Date)

                            return(out4)
                        }
                    )
                )

                # head(data.out)
                # tail(data.out)
                return(data.out)
            }
        )
    )

#head(data)
#tail(data)



# x2 <- do.call(
#     "rbind",
#     pbmclapply(
#         1:uniqueN(data$aphiaID),
#         function(y) {
#             print(y)
#             o <- worrms::wm_record(as.numeric(unique(data$aphiaID)[y]))
#             return(o)
#         }
#     )
# )
# head(as.data.frame(x2))
# x2$AphiaID


# sort(table(data$scientificName))/uniqueN(data$eventID)
data$Param <- CodeTaxon[match(data$scientificName, CodeTaxon$Taxons), "Code"]

# 

sort(unique(data$Param))

# Je ne garde que les animaux
data <- data[data$Param %in% CodeTaxon[CodeTaxon$Kingdom == "Animalia","Code"],]
colnames(data)[colnames(data) == "Abondance"] <- "Val"
Copepods <- data[data$Param %in% CodeTaxon[CodeTaxon$Class == "Copepoda", "Code"], ]

Size <- Copepods
summary(Copepods$Biovolume)
# Pour le moment : Je pose arbitrairement le seuil grand / petit Ã  70mm3/m3 (q75)
# Un bazard de 2mm / 0.5mm a un biovolume d'environ
# BV = pi*L*(1/2*H)^2 #https://www.researchgate.net/post/How-to-measure-the-copepod-biovolume-using-body-measurements

# BV = pi*2*(0.5*0.5)^2
# BV = 0.4mm3
    return(
        list(
            UT = data,
            Copepods = Copepods
           # Size = Size
        )
    )


}


