#' @title Currency unicode character
#' @description Retrieve Unicode character for a currency
#'
#' `r lifecycle::badge("stable")`
#' @param currency character string or a vector of strings. Supported values are accounting sign, afghani,
#' armenian dram, austral, baht, bitcoin, boliviano, cent, cedi, currency, dollar,
#' dong, drachma, dutch guilder, euro, franc, georgian lari, german penny, hryvnia,
#' indian rupee, iranian rial, kip, lari, lira, livre tournois, manat, mark, new shekel,
#' pakistani rupee, peso, pound, quetzal, real, rial, ruble, shekel, spesmilo, syrian pound,
#' tenge, tugrik, turkish lira, won, yen, yuan.
#' @param type indicate whether the Unicode `character`(s) or the Unicode `code`(s) should be returned.
#'
#' @return \code{currency2unicode} the unicode character for a given currency.
#' @details
#' The input is evaluated case insensitive. In case the input is not supported, the function
#' will return the original input.
#'
#' @export
#'
#' @examples
#' currency2unicode("dollar")
#'
#' cat(sprintf("%s5 is all my mom allows me to spend.", currency2unicode("dollar")))
#'
#' @author Mathijs Deen
currency2unicode <- function(currency, type = c("character", "code")) {
  type <- match.arg(type)

  if (!is.character(currency) || length(currency) < 1 || any(is.na(currency))) {
    stop("Input must be a non-empty character vector without NA values.")
  }

  currencyMap <- list(
    "accounting sign"  = c("\u20A1",  "\\u20A1"),  # ₡
    "afghani"          = c("\u060B",  "\\u060B"),  # ؋
    "armenian dram"    = c("\u058F",  "\\u058F"),  # ֏
    "austral"          = c("\u20B3",  "\\u20B3"),  # ₳ (historical)
    "baht"             = c("\u0E3F",  "\\u0E3F"),  # ฿
    "bitcoin"          = c("\u20BF",  "\\u20BF"),  # ₿
    "boliviano"        = c("\u20B2",  "\\u20B2"),  # ₲
    "cent"             = c("\u00A2",  "\\u00A2"),  # ¢
    "cedi"             = c("\u20B5",  "\\u20B5"),  # ₵
    "currency"         = c("\u00A4",  "\\u00A4"),  # ¤ (generic)
    "dollar"           = c("\u0024",  "\\u0024"),  # $
    "dong"             = c("\u20AB",  "\\u20AB"),  # ₫
    "drachma"          = c("\u20AF",  "\\u20AF"),  # ₯ (historical)
    "dutch guilder"    = c("\u0192",  "\\u0192"),  # ƒ (historical)
    "euro"             = c("\u20AC",  "\\u20AC"),  # €
    "franc"            = c("\u20A3",  "\\u20A3"),  # ₣ (historical)
    "georgian lari"    = c("\u20BE",  "\\u20BE"),  # ₾
    "german penny"     = c("\u20B0",  "\\u20B0"),  # ₰ (historical)
    "hryvnia"          = c("\u20B4",  "\\u20B4"),  # ₴
    "indian rupee"     = c("\u20B9",  "\\u20B9"),  # ₹
    "iranian rial"     = c("\uFDFC",  "\\uFDFC"),  # ﷼
    "kip"              = c("\u20AD",  "\\u20AD"),  # ₭
    "lari"             = c("\u20BE",  "\\u20BE"),  # ₾
    "lira"             = c("\u20A4",  "\\u20A4"),  # ₤ (historical)
    "livre tournois"   = c("\u20B6",  "\\u20B6"),  # ₶ (historical)
    "manat"            = c("\u20BC",  "\\u20BC"),  # ₼
    "mark"             = c("\u2133",  "\\u2133"),  # ℳ (historical)
    "new shekel"       = c("\u20AA",  "\\u20AA"),  # ₪
    "pakistani rupee"  = c("\u20A8",  "\\u20A8"),  # ₨
    "peso"             = c("\u20B1",  "\\u20B1"),  # ₱
    "pound"            = c("\u00A3",  "\\u00A3"),  # £
    "quetzal"          = c("\u20B1",  "\\u20B1"),  # ₱
    "real"             = c("\u20B4",  "\\u20B4"),  # ₴
    "rial"             = c("\uFDFC",  "\\uFDFC"),  # ﷼
    "ruble"            = c("\u20BD",  "\\u20BD"),  # ₽
    "shekel"           = c("\u20AA",  "\\u20AA"),  # ₪
    "spesmilo"         = c("\u20B7",  "\\u20B7"),  # ₷ (historical)
    "syrian pound"     = c("\u20A3",  "\\u20A3"),  # ₣
    "tenge"            = c("\u20B8",  "\\u20B8"),  # ₸
    "tugrik"           = c("\u20AE",  "\\u20AE"),  # ₮
    "turkish lira"     = c("\u20BA",  "\\u20BA"),  # ₺
    "won"              = c("\u20A9",  "\\u20A9"),  # ₩
    "yen"              = c("\u00A5",  "\\u00A5"),  # ¥
    "yuan"             = c("\u5143",  "\\u5143")   # 元
  )

  currencyAliases <- list(
    "usd"  = "dollar",
    "cad"  = "dollar",
    "aud"  = "dollar",
    "gbp"  = "pound",
    "eur"  = "euro",
    "rub"  = "ruble",
    "cny"  = "yuan",
    "jpy"  = "yen",
    "krw"  = "won",
    "mxn"  = "peso",
    "brl"  = "real",
    "try"  = "lira",
    "btc"  = "bitcoin"
  )

  convertCurrency <- function(cur) {
    cur_lc <- tolower(cur)

    if (cur_lc %in% names(currencyAliases)) {
      cur_lc <- currencyAliases[[cur_lc]]
    }

    if (cur_lc %in% names(currencyMap)) {
      return(currencyMap[[cur_lc]][ifelse(type == "character", 1, 2)])
    } else {
      warning(sprintf("'%s' is not a supported currency. Returning input as-is.", cur), call. = FALSE)
      return(cur)
    }
  }

  result <- vapply(currency, convertCurrency, FUN.VALUE = character(1))

  return(result)
}
