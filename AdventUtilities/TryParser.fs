namespace AdventUtilities


open System

module TryParser =
    let tryParseWith (tryParseFunction: string -> bool * _) =
        tryParseFunction
        >> function
            | true, v -> Some v
            | false, _ -> None

    [<AutoOpen>]
    module DateTimeConversion =
        //functions
        let parseDate = tryParseWith DateTime.TryParse
        let parseDateTimeOffset = tryParseWith DateTimeOffset.TryParse

        //active patterns
        let (|Date|_|) = parseDate
        let (|DateTimeOffset|_|) = parseDateTimeOffset


    [<AutoOpen>]
    module NumberConversion =
        //functions
        let parseInt = tryParseWith Int32.TryParse

        let parseSingle =
            tryParseWith Single.TryParse >> Option.filter System.Single.IsFinite

        let parseDouble = tryParseWith System.Double.TryParse
        let parseBigInteger = tryParseWith System.Numerics.BigInteger.TryParse
        let parseByte = tryParseWith System.Byte.TryParse
        let parseSByte = tryParseWith System.SByte.TryParse
        let parseDecimal = tryParseWith System.Decimal.TryParse

        //active patterns
        let (|Int|_|) = parseInt
        let (|Single|_|) = parseSingle
        let (|Double|_|) = parseDouble
        let (|BigInteger|_|) = parseBigInteger
        let (|Byte|_|) = parseByte
        let (|SByte|_|) = parseSByte
        let (|Decimal|_|) = parseDecimal
