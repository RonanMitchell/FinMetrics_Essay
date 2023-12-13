Data_Merger_foo <- function(df1, df2, y, y_name){


    wide_data1 <-

        df1 %>%

        select(date, Name, {{y}}) %>%

        spread(key = Name, value = {{y}})


    wide_data2 <-

        df2 %>%

        select(date, Name, {{y}}) %>%

        spread(key = Name, value = {{y}})

    ###

    Main_Data <-

        left_join(wide_data1, wide_data2, "date") %>%

        pivot_longer(cols = -date,
                     names_to = "Name",
                     values_to = {{y_name}})  %>%

        arrange(Name)

    ###

    return(Main_Data)

}