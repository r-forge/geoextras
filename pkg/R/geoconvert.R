geoconvert <-
function (data, inverse = FALSE, col.names = c("lat", "lon")) 
{
    if (!inverse) {
        if (is.data.frame(data)) {
            if (any(is.na(match(col.names, names(data))))) {
                cat(paste("Columns", colnames, "do not exist"))
                return(invisible())
            }
            data[, col.names[1]] <- geoconvert.1(data[, col.names[1]])
            data[, col.names[2]] <- geoconvert.1(data[, col.names[2]])
        }
        else data <- geoconvert.1(data)
    }
    else {
        if (is.data.frame(data)) {
            if (any(is.na(match(col.names, names(data))))) {
                cat(paste("Columns", colnames, "do not exist"))
                return(invisible())
            }
            data[, col.names[1]] <- geoconvert.2(data[, col.names[1]])
            data[, col.names[2]] <- geoconvert.2(data[, col.names[2]])
        }
        else data <- geoconvert.2(data)
    }
    return(data)
}
