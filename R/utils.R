

## FUNCTION: convert girth from inches to cm 
convert_in_to_cm <- function(x) {
    x * 2.54
}

## FUNCRION: convert height from feet to m 
convert_ft_to_m <- function(x) {
    x * .3048
}

## FUNCTION: calculate volume in m^3
calculate_volume <- function(diameter, height) {
    pi / 4 * (diameter/100)^2 * height
}

## FUNCTION: summarise iris data set
cal_iris_mean <- function(data) {
    data %>% 
        summarise(
            across(
                where(is.numeric), mean
            ), 
            .by = Species
        ) %>% 
        pivot_longer(
            col = where(is.numeric), 
            names_to = "measure",
            values_to = "mean"
        ) %>% 
        group_by(measure) %>% 
        arrange(
            desc(mean), 
            .by_group = TRUE
        ) %>% 
        ungroup()
}
