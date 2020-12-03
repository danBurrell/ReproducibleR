if(!require(pacman)) install.packages("pacman")
library(pacman)

p_load(here)
main_dir = paste(here(), "QCIF", sep="/")
data_dir = "data"
data_file = "gapminder_data.csv"

gapminder = read.csv(
    file = paste(main_dir, data_dir, data_file, sep="/"), 
    header = TRUE, 
    sep = ",",          
    dec = ".", 
    stringsAsFactors = TRUE)

p_load(dplyr)
glimpse(gapminder)

# Create function: F to C and back
temperature_converter = function(temperature, from_scale = c("F", "C", "K"), to_scale = c("C", "F", "K")){
    F2C = function(ft){
        ct = (5/9) * (ft - 32)
        return(ct)
    }
    C2F = function(ct){
        ft = (9/5)*ct + 32
        return(ft)
    }
    C2K = function(ct){
        kt = ct + 273.15
        return(kt)
    }
    K2C = function(kt){
        ct = kt - 273.15
        return(ct)
    }
    
    if(from_scale == "F"){
        if(!is.numeric(temperature)){stop("temperature is not a numeric vector")}
        if(temperature < -459.67){stop("temperature is below absolute zero")}
        
        ifelse(
            test = (to_scale == "K"),
            yes = (out = C2K(F2C(temperature))),
            no = (out = F2C(temperature) ) )
    } else if(from_scale == "C"){
        if(!is.numeric(temperature)){stop("temperature is not a numeric vector")}
        if(temperature < -273.15){stop("temperature is below absolute zero")}
            
        ifelse(
            test = (to_scale == "K"),
            yes = (out = C2K(temperature)),
            no = (out = C2F(temperature) ) )
        
    } else{
        if(!is.numeric(temperature)){stop("temperature is not a numeric vector")}
        if(temperature < 0){stop("temperature is below absolute zero")}
        
        ifelse(
            test = (to_scale == "C"),
            yes = (out = K2C(temperature)),
            no = (out = C2F(K2C(temperature))) ) 
    
    return(out)
}
    
temperature_converter(0, "C", "F")

gapminder %>%
    group_by(continent, year) %>%
    summarise(mean_gdpPercap = mean(gdpPercap))

# count() and n()
gapminder %>%
    filter(year == 2002) %>%
    count(continent, sort = TRUE)

gapminder %>%
    group_by(continent) %>%
    summarise(mean_lifeExp = mean(lifeExp),
              se_lifeExp =  sd(lifeExp)/sqrt(n()))


# combining dplyr and ggplot
p_load(ggplot2)

starts.with = substr(gapminder$country, start = 1, stop = 1)
az.country = gapminder[starts.with %in% c("A", "Z"),]
az.country

az.country %>% 
    ggplot(mapping = aes(x=year, y=lifeExp, colour = continent)) +
    geom_point() +
    geom_smooth(method = "gam") +
    facet_wrap(~country)

gapminder %>%
    filter(substr(country,1,1) %in% c("A","Z") ) %>%
    ggplot(mapping = aes(x=year, y=lifeExp, colour = continent)) +
    geom_point() +
    geom_smooth(method = "gam") +
    facet_wrap(~country) +
    theme(legend.position = "none")

gapminder %>%
    filter(year == 2002) %>%
    group_by(continent) %>%
    slice_sample(n=2) %>%
    summarize(mean_lifeExp = mean(lifeExp)) %>%
    arrange(desc(mean_lifeExp))
    
pdf("Life_Exp_vs_time.pdf", width=12, height=4)  ## or jpeg, or png
ggplot(gapminder, aes(x=year, y=lifeExp, colour = country)) + 
    geom_line() +
    theme(legend.position = "none")
dev.off()
