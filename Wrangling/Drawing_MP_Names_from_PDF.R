## Packages and Themes
# Install the packages and set the theme
library(tidyverse)
library(pdftools)
library(stringr)
library(curl)
library(readxl)
library(sysfonts)
library(showtext)

font_add_google("Spline Sans")
showtext_auto()

theme_clean <- theme_bw(base_family = "Spline Sans") + 
  theme(legend.position = "top",
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 24, face = "bold"),
        plot.subtitle = element_text(size = 20, face = "italic",
                                     margin = margin(b=12)),
        plot.caption = element_text(size = 20,
                                    vjust = -1),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.title.position = "plot",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = 20, face = "bold"))
theme_set(theme_clean)

## The file is available on the House of Commons website
pdf_path <- "https://commonsbusiness.parliament.uk/Document/88504/Pdf"

# Draw the text values using the PDFtools functions
pdf_info <- pdftools::pdf_info(pdf_path)
pdf_text <- pdftools::pdf_text(pdf_path)

# This custom function defines what we do with each input
hoc_table_converter <- function(input_pdf_text, input_pdf_rows){
  output_df <- tibble(raw_text = input_pdf_text) %>%
  dplyr::slice(input_pdf_rows) %>%
  tidyr::separate(raw_text,
                  into = c("m1", "m2", "m3"),
                  sep = "(?<=[a-z-])\\s{2,}") %>%
  utils::stack() %>%
  dplyr::mutate(member_column = trimws(values, which = "both")) %>%
  dplyr::select(member_column) %>%
  tidyr::separate(member_column,
                  into = c("entry_number", "member_name"),
                  sep = "(?<=[0-9])\\s",
                  fill = "left") %>%
  dplyr::mutate(en_lead = lead(entry_number),
                mn_lead = lead(member_name)) %>%
  dplyr::mutate(mp_name =
                  dplyr::if_else(is.na(en_lead) & !is.na(mn_lead),
                                 paste(member_name, mn_lead),
                                 member_name)) %>%
  tidyr::drop_na(entry_number) %>%
  dplyr::select(entry_number, mp_name)
  return(output_df)}

# Page 1
hoc_pg1 <- stringr::str_split(pdf_text[[1]], "\\n", simplify = TRUE) %>%
  as.vector()
hoc_page1_df <- hoc_table_converter(hoc_pg1, 7:43)

# Page 2
hoc_pg2 <- stringr::str_split(pdf_text[[2]], "\\n", simplify = TRUE) %>%
  as.vector()
hoc_page2_df <- hoc_table_converter(hoc_pg2, 4:52)

##Page 3
hoc_pg3 <- stringr::str_split(pdf_text[[3]], "\\n", simplify = TRUE) %>%
  as.vector()
hoc_page3_df <- hoc_table_converter(hoc_pg3, 4:52)

# Page 4
hoc_pg4 <- stringr::str_split(pdf_text[[4]], "\\n", simplify = TRUE) %>%
  as.vector()
hoc_page4_df <- hoc_table_converter(hoc_pg4, 4:33)

## Create the full list
titles_list <- c("Mr |Mrs |Ms |Dr |Dame |Sir ")

# Trim the names, removing titles
hoc_list_df <- dplyr::bind_rows(
  hoc_page1_df, hoc_page2_df, hoc_page3_df, hoc_page4_df) %>%
  dplyr::mutate(entry_number = as.numeric(entry_number),
                member_parl_name = str_replace_all(trimws(mp_name),
                                                   pattern = titles_list,
                                                   replacement = ""),
                member_parl_name = str_replace_all(member_parl_name,
                                                   pattern = "- ",
                                                   replacement = "-")) %>%
  dplyr::arrange(entry_number)

## Now, we join to a list of elected MPs
hocl_url <- "https://researchbriefings.files.parliament.uk/documents/CBP-10009/MPs-elected.xlsx"
hocl_sheet <- "Sheet1"
hocl_range <- "A1:P651"

temp <- tempfile()
temp <- curl::curl_download(
  url = hocl_url, destfile = temp,
  quiet = TRUE, mode = "wb")

# There are 11 MPs that do not match to the House of Commons list
# This is corrected in the latter column
ge2024_mp_df <- read_excel(temp,
                           sheet = hocl_sheet,
                           range = hocl_range) %>%
  dplyr::mutate(elected_mp_name = paste(firstname, surname),
                elected_mp_name_corrected =
  case_when(elected_mp_name == "Charles Maynard" ~ "Charlie Maynard",
            elected_mp_name == "Rebecca Long-Bailey" ~ "Rebecca Long Bailey",
            elected_mp_name == "Tan Dhesi" ~ "Tanmanjeet Singh Dhesi",
            elected_mp_name == "Sorcha-Lucy Eastwood" ~ "Sorcha Eastwood",
            elected_mp_name == "Preet Gill" ~ "Preet Kaur Gill",
            elected_mp_name == "Steffan Aquarone" ~ "Steff Aquarone",
            elected_mp_name == "Daniel Aldridge" ~ "Dan Aldridge",
            elected_mp_name == "Mary Foy" ~ "Mary Kelly Foy",
            elected_mp_name == "Freddie Van Mierlo" ~ "Freddie van Mierlo",
            elected_mp_name == "Joshua Robert Abraham Dean" ~ "Josh Dean",
            elected_mp_name == "Greg Stafford" ~ "Gregory Stafford",
            TRUE ~ elected_mp_name),
  party_name_grouped =
    case_when(party_name == "Labour and Co-operative" ~ "Labour",
              TRUE ~ party_name))

## Summary table and graph
uk_party_colours = c("#F6CB2F", "#0087DC", "#D46A4C",
                     "#02A95B", "#FF66A1", "#E4003B",
                     "#FAA61A", "#005B54", "#12B6CF",
                     "#FDF38E", "#2AA82C", "#0C3A6A", "#48A5EE")

ballot_party_title <- "112 Conservative MPs were in the ballot for the Private Members' Bill in 2024."
ballot_party_subtitle <- "Count by party of Members of Parliament entering into the Private Members' Bill ballot. In total, 458 MPs entered into the draw, which took place on Thursday 05 September 2024."
ballot_party_caption <- "Sources: UK Parliament documents (88504/Pdf) and House of Commons Library (CBP-10009)."

ballot_party_df <- dplyr::left_join(
  hoc_list_df, ge2024_mp_df,
  by = c("member_parl_name" = "elected_mp_name_corrected")) %>%
  dplyr::group_by(party_name_grouped) %>%
  dplyr::summarise(count_of_members = n()) %>%
  dplyr::arrange(-count_of_members)

# Graph
ballot_party_gg <- ballot_party_df %>%
  ggplot(aes(y = reorder(party_name_grouped, count_of_members),
             x = count_of_members)) +
  geom_col(aes(fill = party_name_grouped)) +
  scale_fill_manual(guide = "none",
                    values = uk_party_colours) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0, 275)) +
  scale_y_discrete(expand = c(0,0)) +
  geom_text(aes(x = count_of_members, y = party_name_grouped,
                label = count_of_members),
            hjust = -0.1, size = 7, fontface = "bold") +
  labs(title = ballot_party_title,
       subtitle = str_wrap(ballot_party_subtitle, width = 120),
       x = "Count of Members",
       y = "Party",
       caption = ballot_party_caption)

## Saving the outputs
png("R/Analysis_2024/04_Wrangling/ballot_party_gg.png",
    width = 2000, height = 1200, unit = "px")
ballot_party_gg
dev.off()