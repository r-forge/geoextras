geoconvert <-
function (data, inverse = F, col.names = c("lat", "lon")) 
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
geoconvert.1 <-
function (x) 
{
    i <- sign(x)
    x <- abs(x)
    x1 <- x%%10000
    k <- c(1:length(x1))
    k <- k[x1 > 5999 & !is.na(x1)]
    if (length(k) > 0) 
        print(paste("error > 60 min nr", k, x[k]))
    min <- (x/100) - trunc(x/10000) * 100
    return((i * (x + (200/3) * min))/10000)
}
geoconvert.2 <-
function (lat) 
{
    i <- sign(lat)
    lat <- abs(lat)
    p1 <- floor(lat)
    p2 <- floor((lat - p1) * 60)
    p3 <- round((lat - p1 - p2/60) * 100 * 60)
    return(i * (p1 * 10000 + p2 * 100 + p3))
}
