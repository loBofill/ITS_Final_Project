createOrdersData <- function(group, packs, products, shipments, pib) {
    
    packs <- fread("Packs.csv")
    products <- fread("Products.csv")
    shipments <- fread("shipments.csv")
    pib <- fread("pib.csv")
    
    set.seed(group * 777)
    
    packs %<>% select(Pack, Description, Price) %>% 
        na.omit %>% 
        mutate(prob = runif(15) / Price, Quantity = prob / sum(prob))
    
    pib %<>% mutate(prob = 1 + runif(56) * PIB, Quantity = prob / sum(prob))
    
    weekQuantities <- 1 + runif(13)
    weekQuantity <- data.frame(Week = 1:13, Quantity = weekQuantities / sum(weekQuantities))
    
    orders <- expand.grid(Week = 1:13, Pack = packs$Pack, Destination = pib$Country) 
    orders %<>%
        left_join(packs, by = 'Pack') %>%
        left_join(pib, by = c('Destination' = 'Country')) %>%
        left_join(weekQuantity, by = 'Week') %>%
        mutate(probs = Quantity * Quantity.x * Quantity.y * abs(rnorm(nrow(orders))),
               Units = round(probs * 10^7 / sum(probs))) %>%
        select(Week, Pack, Destination, Units) %>%
        filter(Units > 0)
    return(orders)
}

correctComma <- function(text) {
    as.numeric(gsub(",", ".", text))
}

getStats <- function(data) {
    return(data %>% summarize(units = round(sum(Units) / 1000),
                       revenue = round(sum(revenue) / 1000),
                       productCost = round(sum(rawCost) / 1000),
                       shipCost = round(sum(shipCost) / 1000),
                       totalCost = shipCost + productCost,
                       shippedWeight = round(sum(totalWeight) / 1000),
                       grossMargin = percent((revenue - totalCost) / revenue)))
}
