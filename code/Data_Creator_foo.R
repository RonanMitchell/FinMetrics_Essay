Data_Creator_foo <- function(data, name_sub, tick_sub){

    {{data}} %>%

        mutate(Name = gsub({{name_sub}}, "", Name)) %>%

        mutate(Ticker = gsub({{tick_sub}}, "", Ticker)) %>%

        filter(date >= "2010-01-01" & date <= "2022-03-31") %>%

        group_by(YearMonth = format(date, "%Y-%m")) %>%

        filter(date == max(date)) %>%

        ungroup()

}