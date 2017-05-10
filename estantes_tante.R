# Funcion para obtener una tabla organizada de los estantes
# estantes de tante == es.tante (jajajajajaj)
################################################################################
# Function(s)
################################################################################

## ---- estantes ---- 
es.tante <- function(e, t){
        t$celdas <- t$Hylder_NY*4
        t_vec <- as.character(rep(t$Teens_navn, t$celdas))
        start <- 1
        estante_list <- list()
        for(a in seq_along(e[,1])){
                cupos <- e$Col[a]*4 * e$Row[a]
                tmp_df <- as.data.frame(matrix(t_vec[start:(start+cupos-1)], ncol=e$Col[a]*4, nrow = e$Row[a], byrow = T))
                colnames(tmp_df) <- rep(seq(1,e$Col[a]), each=4)
                start <- start + cupos
                estante_list[[a]] <- tmp_df
        }
        estante_list
}

################################################################################
# Data and packages required 
################################################################################
e <- estantes_df <- read.delim("estantes.txt", header = T, sep = "\t")
t <- tes_df <- read.delim("tes.txt", header = T, sep = "\t")

################################################################################
# Script
################################################################################
estantes_lilia <- es.tante(e, t)


################################################################################
# Para imprimir
################################################################################
# esta seria la primera tabla

estantes_lilia[[1]]
