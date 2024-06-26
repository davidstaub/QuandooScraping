% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/QuandooScraping.R
\name{QuandooScraping}
\alias{QuandooScraping}
\title{Scrape and Enhance Quandoo Restaurant Reviews}
\usage{
QuandooScraping(
  url = "https://www.quandoo.ch/en/place/restaurant-sporrer-61089"
)
}
\arguments{
\item{url}{The URL to the Quandoo restaurant page from which the reviews are to be retrieved.}
}
\value{
A df with the following structure:
(name|review_count|date|sex|prob_male|rating|description|description_lang)
}
\description{
This function is designed for scraping restaurant reviews from Quandoo and augmenting them with additional insights.
It retrieves user-generated content and enriches this data with valuable information such as calculated review dates,
estimated gender of the reviewers, and detected review languages. Gender estimation is performed using the \code{gender::gender()}
function. Language detection is accomplished with \code{cld2::detect_language()}.
Load the sample dataset with 1444 reviews for a restaurant in zurich:\code{data(review_example)} (?review_example)
}
\examples{
review_df <- QuandooScraping()
review_df <- QuandooScraping(url="https://www.quandoo.ch/en/place/yens-restaurant-11833")
}
