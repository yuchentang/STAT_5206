# HW7: nycstock
#
# In this exercise, you will conduct a data analysis on NYC stock prices.
# The dataset consists of following files:
#   - `prices.csv`: daily prices, with most of data spanning from 2010 to the
#     end of 2016 (for newer companies, the date range is shorter).
#     The dataset doesn't account for stock splits.
#   - `securities.csv`: general description of each company (e.g., sector).
#   - `fundamentals.csv`: metrics extracted from annual SEC 10K fillings
#     (2012-2016), which allows to derive most of the popular fundamental
#     indicators.
#
# In the whole exercise, do NOT use a `for`, `while`, or `repeat` loop!
#
# 1. Load the `tidyverse` and `lubridates` packages and use
#    `theme_set(theme_light())` to set the theme for the rest of the exercise.
#    Then, use `read_csv` to load
#      - `/course/data/prices.csv` (with `locale = locale(tz = "America/New_York")`
#        to set the timezone properly) and assign it to a tibble `prices`,
#      - `/course/data/securities.csv` and assign it to a tibble `securities`,
#      - `/course/data/fundamentals.csv` and assign it to a tibble `fund`.
#
#    To check your answer:
#
#    The output of `print(prices, n = 5)` is
#
#    ```
#    # A tibble: 851,264 x 7
#      date                symbol  open close   low  high  volume
#      <dttm>              <chr>  <dbl> <dbl> <dbl> <dbl>   <dbl>
#    1 2016-01-05 00:00:00 WLTW    123.  126.  122.  126. 2163600
#    2 2016-01-06 00:00:00 WLTW    125.  120.  120.  126. 2386400
#    3 2016-01-07 00:00:00 WLTW    116.  115.  115.  120. 2489500
#    4 2016-01-08 00:00:00 WLTW    115.  117.  114.  117. 2006300
#    5 2016-01-11 00:00:00 WLTW    117.  115.  114.  117. 1408600
#    # … with 8.513e+05 more rows
#    ```
#
#    The output of `print((prices %>% pull(date))[1:3])` is
#
#    ```
#    [1] "2016-01-05 EST" "2016-01-06 EST" "2016-01-07 EST"
#    ```
#
#    The output of `print(securities, n = 5)` is
#
#    ```
#    # A tibble: 505 x 8
#       Ticker symbol  Security  SEC filings   GICS Sector
#      <chr>           <chr>    <chr>         <chr>
#    1 MMM             3M Comp… reports       Industrials
#    2 ABT             Abbott … reports       Health Care
#    3 ABBV            AbbVie   reports       Health Care
#    4 ACN             Accentu… reports       Information …
#    5 ATVI            Activis… reports       Information …
#    # … with 500 more rows, and 4 more variables:  GICS Sub
#    #   Industry  <chr>,  Address of Headquarters  <chr>,  Date first
#    #   added  <date>, CIK <chr>
#    ```
#
#    The output of `print(fund, n = 5)` is
#
#    ```
#    # A tibble: 1,781 x 78
#       Ticker Symbol   Period Ending   Accounts Payab…  Accounts Recei…
#      <chr>           <chr>                      <dbl>            <dbl>
#    1 AAL             12/31/12              3068000000       -222000000
#    2 AAL             12/31/13              4975000000        -93000000
#    3 AAL             12/31/14              4668000000       -160000000
#    4 AAL             12/31/15              5102000000        352000000
#    5 AAP             12/29/12              2409453000        -89482000
#    # … with 1,776 more rows, and 74 more variables:  Add'l
#    #   income/expense items  <dbl>,  After Tax ROE  <dbl>,  Capital
#    #   Expenditures  <dbl>,  Capital Surplus  <dbl>,  Cash Ratio  <dbl>,
#    #    Cash and Cash Equivalents  <dbl>,  Changes in
#    #   Inventories  <dbl>,  Common Stocks  <dbl>,  Cost of
#    #   Revenue  <dbl>,  Current Ratio  <dbl>,  Deferred Asset
#    #   Charges  <dbl>,  Deferred Liability Charges  <dbl>,
#    #   Depreciation <dbl>,  Earnings Before Interest and Tax  <dbl>,
#    #    Earnings Before Tax  <dbl>,  Effect of Exchange Rate  <dbl>,
#    #    Equity Earnings/Loss Unconsolidated Subsidiary  <dbl>,  Fixed
#    #   Assets  <dbl>, Goodwill <dbl>,  Gross Margin  <dbl>,  Gross
#    #   Profit  <dbl>,  Income Tax  <dbl>,  Intangible Assets  <dbl>,
#    #    Interest Expense  <dbl>, Inventory <dbl>, Investments <dbl>,
#    #   Liabilities <dbl>,  Long-Term Debt  <dbl>,  Long-Term
#    #   Investments  <dbl>,  Minority Interest  <dbl>,  Misc.
#    #   Stocks  <dbl>,  Net Borrowings  <dbl>,  Net Cash Flow  <dbl>,
#    #    Net Cash Flow-Operating  <dbl>,  Net Cash
#    #   Flows-Financing  <dbl>,  Net Cash Flows-Investing  <dbl>,  Net
#    #   Income  <dbl>,  Net Income Adjustments  <dbl>,  Net Income
#    #   Applicable to Common Shareholders  <dbl>,  Net Income-Cont.
#    #   Operations  <dbl>,  Net Receivables  <dbl>,  Non-Recurring
#    #   Items  <dbl>,  Operating Income  <dbl>,  Operating Margin  <dbl>,
#    #    Other Assets  <dbl>,  Other Current Assets  <dbl>,  Other
#    #   Current Liabilities  <dbl>,  Other Equity  <dbl>,  Other
#    #   Financing Activities  <dbl>,  Other Investing Activities  <dbl>,
#    #    Other Liabilities  <dbl>,  Other Operating Activities  <dbl>,
#    #    Other Operating Items  <dbl>,  Pre-Tax Margin  <dbl>,  Pre-Tax
#    #   ROE  <dbl>,  Profit Margin  <dbl>,  Quick Ratio  <dbl>,  Research
#    #   and Development  <dbl>,  Retained Earnings  <dbl>,  Sale and
#    #   Purchase of Stock  <dbl>,  Sales, General and Admin.  <dbl>,
#    #    Short-Term Debt / Current Portion of Long-Term Debt  <dbl>,
#    #    Short-Term Investments  <dbl>,  Total Assets  <dbl>,  Total
#    #   Current Assets  <dbl>,  Total Current Liabilities  <dbl>,  Total
#    #   Equity  <dbl>,  Total Liabilities  <dbl>,  Total Liabilities &
#    #   Equity  <dbl>,  Total Revenue  <dbl>,  Treasury Stock  <dbl>,
#    #    For Year  <dbl>,  Earnings Per Share  <dbl>,  Estimated Shares
#    #   Outstanding  <dbl>
#    ```
#
## Do not modify this line!
library(tidyverse)
library(lubridate)
theme_set(theme_light())
prices <- read_csv("prices.csv", locale = locale(tz = "America/New_York"))
securities <- read_csv("securities.csv")
fund <- read_csv("fundamentals.csv")

# 2. In `securities`, let's keep the companies belonging to the top 6 sectors
#    (by frequency of occurence), as well as those whose `GICS Sub Industry`
#    falls into `"Gold"` or `"Real Estate`" (i.e., `GICS Sub Industry` contains
#    either `"Gold"` or `"REITs"`).
#    You need to do it in two steps:
#      - First, create a tibble named `securities_sectored` that contain
#        only the companies that do not belong to those that you want (see
#        below).
#        Note that `securities_sectored` should contain an additional column
#        `GICS Sector truncated` that contains the top 6 factors in `GICS Sector`
#        and all the others lumped into an additional level `"Other"`.
#      - Second, use `anti_join()` on `securities` and `securities_sectored` to
#        create `securities_selected`, which contains only the rows that
#        meet the requirements above by deleting the rows from `securities`
#        that are in `securities_sectored`.
#        Then, use `select()` to only select the columns `Ticker symbol`,
#        `Security`, and `GICS Sector`.
#    To extract `securities_sectored` from `securities`, you can use:
#        - `mutate()` along with `fct_infreq()` and `fct_lump()` to
#          reorder the sectors by frequency of occurence and lump
#          all except the top 6 into a single level `"Other"`.
#        - `filter()` to select rows such that
#          - `GICS Sector truncated` do not belong to the top 6 (i.e., the ones
#            with the level `"Other"`),
#          - `GICS Sub Industry` does not contain the words `"Gold"` or
#            `"REITs"` (e.g., with `str_detect()` using a regular expression
#            with OR represented by the alternation symbol `"|"`).
#    To check your answer:
#
#    The output of `print(securities_sectored, n = 5)` is
#
#    ```
#    # A tibble: 94 x 9
#       Ticker symbol  Security  SEC filings   GICS Sector
#      <chr>           <chr>    <chr>         <chr>
#    1 AES             AES Corp reports       Utilities
#    2 APD             Air Pro… reports       Materials
#    3 ALB             Albemar… reports       Materials
#    4 LNT             Alliant… reports       Utilities
#    5 AEE             Ameren … reports       Utilities
#    # … with 89 more rows, and 5 more variables:  GICS Sub
#    #   Industry  <chr>,  Address of Headquarters  <chr>,  Date first
#    #   added  <date>, CIK <chr>,  GICS Sector truncated  <fct>
#    ```
#
#    The output of `print(securities_selected, n = 5)` is
#
#    ```
#    # A tibble: 411 x 3
#       Ticker symbol  Security             GICS Sector
#      <chr>           <chr>               <chr>
#    1 MMM             3M Company          Industrials
#    2 ABT             Abbott Laboratories Health Care
#    3 ABBV            AbbVie              Health Care
#    4 ACN             Accenture plc       Information Technology
#    5 ATVI            Activision Blizzard Information Technology
#    # … with 406 more rows
#    ```
#
## Do not modify this line!
securities_sectored <- securities %>%
  mutate(GICS.Sector.truncated = fct_lump_n(GICS.Sector, 6)) %>%
  filter((GICS.Sector.truncated == "Other") & (!str_detect(GICS.Sub.Industry, "Gold|REITs")))
securities_selected <- securities %>%
  anti_join(securities_sectored) %>%
  select(Ticker.symbol, Security, GICS.Sector)


# 3. From `fund`, extract a tibble `fund_time` by converting the column name
#    of `fund` from `Ticker Symbol` to `Ticker symbol` (for consistency between
#    the column names of the different tables).
#    Then:
#      - create new column `Period Ending Year` by extract the year from
#        `Period Ending`
#      - drop the rows containing `NA` values,
#      - select the columns `Ticker symbol`, `Period Ending Year` and
#        `Gross Margin`.
#    To do that, you can use:
#      - `rename()` to convert the column name,
#      - `mutate()`, `mdy()` and `year()` to create `Period Ending Year`,
#      - `drop_na()` to drop the rows that contain `NA` values.
#      - `dplyr::select()` to select the required columns.
#
#    To check your answer:
#
#    The output of `print(fund_time, n = 5)` is
#
#    ```
#    # A tibble: 1,299 x 3
#       Ticker symbol   Period Ending Year   Gross Margin
#      <chr>                          <dbl>          <dbl>
#    1 AAL                             2012             58
#    2 AAL                             2013             59
#    3 AAL                             2014             63
#    4 AAL                             2015             73
#    5 AAP                             2012             50
#    # … with 1,294 more rows
#    ```
#
## Do not modify this line!
fund_time <- fund %>%
  rename(Ticker.symbol = Ticker.Symbol) %>%
  mutate(Period.Ending.Year = year(mdy(Period.Ending))) %>%
  drop_na() %>%
  select(Ticker.symbol, Period.Ending.Year, Gross.Margin)


# 4. Extract a tibble `securities_fund` by joining `securities_selected`
#    and `fund_time`: `securities_fund` should contain all the rows in
#    `securities_selected` where there is a match in `fund_time`, using
#    `Ticker symbol` as key.
#
#    To check your answer:
#
#    The output of `print(securities_fund, n = 5)` is
#
#    ```
#    # A tibble: 988 x 5
#       Ticker symbol  Security  GICS Sector   Period Ending …
#      <chr>           <chr>    <chr>                    <dbl>
#    1 MMM             3M Comp… Industrials               2013
#    2 MMM             3M Comp… Industrials               2014
#    3 MMM             3M Comp… Industrials               2015
#    4 ABT             Abbott … Health Care               2012
#    5 ABT             Abbott … Health Care               2013
#    # … with 983 more rows, and 1 more variable:  Gross Margin  <dbl>
#    ```
#
## Do not modify this line!
securities_fund <- securities_selected %>%
  inner_join(fund_time, by = "Ticker.symbol")


# 5. Generate histograms of the `Gross Margin` by facceted by sector.
#    To do this, use `securities_fund` and:
#      - `geom_histogram()` with `binwidth = 10`,
#      - `facet_wrap()` to facet by `GICS Sector`,
#      - `labs()` to set the
#        - title as `"The gross margin distribution varies by sector"`,
#        - x-axis as `"Gross Margin (%)"`
#        - y-axis as `"Count (n)"`.
#    Store the plot into a `ggplot` object `gross_margin`.
#
## Do not modify this line!
gross_margin <- ggplot(securities_fund, aes(Gross.Margin)) + 
  geom_histogram(binwidth = 10) + 
  facet_wrap(~ GICS.Sector) + 
  labs(title = "The gross margin distribution varies by sector", x = "Gross Margin (%)", y = "Count (n)")


# 6. Let's now look at stock prices.
#    First, from `securities_fund`, extract a tibble `tickers_sectors` that
#    contains the `Ticker symbol`, `Security`, and `GICS Sector` columns,
#    keeping only the `unique()` combinations and `arrange()` them by
#    `Ticker symbol`.
#    Second, from `prices`, extract a tibble `full_stock` that contains the
#    `Ticker symbol` (renamed from `symbol`), `close`, `open`, `date`,
#    and `year` (extracted from `date`), and add it the `Security` and
#    `GICS Sector` information from `tickers_sectors`.
#    Filter out the rows where the `GICS Sector` is not available.
#    To do that, you can use:
#       - `rename()` to conver `symbol` into `Ticker symbol`,
#       - `mutate()` and `year()` to extract the year,
#       - `dplyr::select()` to select the relevant variables
#       - `left_join()` to add the security name and sector,
#       - `filter()` to delete the rows where the sector information is
#         not available.
#
#    To check your answer:
#
#    The output of `print(tickers_sectors, n = 5)` is
#
#    ```
#    # A tibble: 276 x 3
#       Ticker symbol  Security                 GICS Sector
#      <chr>           <chr>                   <chr>
#    1 AAL             American Airlines Group Industrials
#    2 AAP             Advance Auto Parts      Consumer Discretionary
#    3 AAPL            Apple Inc.              Information Technology
#    4 ABBV            AbbVie                  Health Care
#    5 ABC             AmerisourceBergen Corp  Health Care
#    # … with 271 more rows
#    ```
#
#    The output of `print(full_stock, n = 5)` is
#
#    ```
#    # A tibble: 468,588 x 7
#       Ticker symbol   close   open date                 year Security
#      <chr>            <dbl>  <dbl> <dttm>              <dbl> <chr>
#    1 AAL               4.77   4.84 2010-01-04 00:00:00  2010 America…
#    2 AAP              40.4   40.7  2010-01-04 00:00:00  2010 Advance…
#    3 AAPL            214.   213.   2010-01-04 00:00:00  2010 Apple I…
#    4 ABC              26.6   26.3  2010-01-04 00:00:00  2010 Ameriso…
#    5 ABT              54.5   54.2  2010-01-04 00:00:00  2010 Abbott …
#    # … with 4.686e+05 more rows, and 1 more variable:  GICS
#    #   Sector  <chr>
#    ```
#
## Do not modify this line!
tickers_sectors <- securities_fund %>%
  group_by(Ticker.symbol, Security, GICS.Sector) %>%
  summarize()
full_stock <- prices %>%
  rename(Ticker.symbol = symbol) %>%
  mutate(year = year(date)) %>%
  left_join(tickers_sectors, by = 'Ticker.symbol') %>%
  select(Ticker.symbol, close, open, date, year, Security, GICS.Sector) %>%
  filter(!is.na(GICS.Sector))


# 7. Plot the evolution to the (closing) stock price in 2010~2016 for the
#    following companies:
#      `"Aetna Inc"`, `"Amazon.com Inc"`, `"Facebook"`, `"Whole Foods Market"`,
#      `"FedEx Corporation"`, `"Boeing Company"`, `"The Walt Disney Company"`.
#    To do that, use:
#      - `filter()` to select the rows of the right companies,
#      - `ggplot()` to initialize the plot,
#      - `geom_line()` to get the lineplot (color by `Security`),
#      - `geom_smooth()` with `method = "loess"` to add a smoothing trend,
#      - `labs()` to set the
#        - title as `"Amazon's stock price more than doubled!"`,
#        - x-axis as `"Date"`
#        - y-axis as `"Daily close price (USD)"`.
#    Store the plot into a `ggplot` object `trend`.
## Do not modify this line!
full_stock_temp <- full_stock %>%
  filter(Security == "Aetna Inc" | Security == "Amazon.com Inc" | Security == "Facebook" | Security == "Whole Foods Market" | Security == "FedEx Corporation" | Security == "Boeing Company" | Security == "The Walt Disney Company")
ggplot(full_stock_temp, aes(date, close, color = Security)) + geom_line() + labs(title = "Amazon's stock price more than doubled!", x = "Date", y = "Daily close price (USD)")

# 10. Calculate the annual "Rate of Return" (RoR, or return) on the securities
#    in `full_stock`. The RoR is defined as the net gain or loss on an
#    investment over a specified time period, calculated as a percentage of the
#    investment's initial cost.
#    Formally, RoR = (current value - initial value) / initial value.
#    From `full_stock`, extract a tibble `return_stock` grouping by
#    `year`, `Ticker symbol`, `GICS Sector`.
#    Then, for year and stock, extract the open corresponding to the first
#    (i.e. `min()`) date, and the close corresponding to the last
#    (i.e. `max()`) date.
#    Finally, compute `return` as `(close - open) / open`.
#    Hint: this can be done simply using `group_by()` and `summarize()`.
#
#    To check your answer:
#
#    The output of `print(return_stock, n = 5)` is
#
#    ```
#    # A tibble: 1,866 x 6
#    # Groups:   year, Ticker symbol [1,866]
#       year  Ticker symbol   GICS Sector             open close return
#      <dbl> <chr>           <chr>                   <dbl> <dbl>  <dbl>
#    1  2010 AAL             Industrials              4.84  10.0  1.07
#    2  2010 AAP             Consumer Discretionary  40.7   66.2  0.625
#    3  2010 AAPL            Information Technology 213.   323.   0.511
#    4  2010 ABC             Health Care             26.3   34.1  0.298
#    5  2010 ABT             Health Care             54.2   47.9 -0.116
#    # … with 1,861 more rows
#    ```
#
## Do not modify this line!



# 11. From `return_stock`, extract a tibble `summary_stock` by calculating
#    the mean, 0.25 quantile, mean, and 0.75 quantile of `return`
#    for each `GICS Sector`.
#    Hint: you can do that using `group_by()` and `summarize()`.
#
#    To check your answer:
#
#    The output of `print(summary_stock, n = 5)` is
#
#    ```
#    # A tibble: 8 x 4
#       GICS Sector                q25 mean_return   q75
#      <chr>                     <dbl>       <dbl> <dbl>
#    1 Consumer Discretionary -0.0357       0.150  0.318
#    2 Consumer Staples        0.00668      0.0902 0.192
#    3 Financials             -0.0286       0.100  0.243
#    4 Health Care            -0.0195       0.150  0.256
#    5 Industrials            -0.0422       0.140  0.299
#    # … with 3 more rows
#    ```
## Do not modify this line!




