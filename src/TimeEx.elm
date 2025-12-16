module TimeEx exposing (durationStr)
import Time

hourInSeconds : Int
hourInSeconds = 60 * 60

minsInSeconds : Int
minsInSeconds = 60

durationStr : Time.Posix -> Time.Posix -> String
durationStr start end =
    let
        totalSecs = ((Time.posixToMillis end) - (Time.posixToMillis start)) // 1000

        hours = totalSecs // hourInSeconds
        mins = (totalSecs - hours * hourInSeconds) // minsInSeconds
        secs = totalSecs - (hours * hourInSeconds) - (mins * minsInSeconds)

        hoursStr = if hours > 0
            then (String.fromInt hours) ++ (if hours > 1 then " hours" else " hour")
            else ""
        minsStr = if mins > 0
            then (String.fromInt mins) ++ (if mins > 1 then " minutes" else " minute")
            else ""
        secsStr = if secs > 0
            then (String.fromInt secs) ++ (if secs > 1 then " seconds" else " second")
            else ""
    in
    [hoursStr, minsStr, secsStr]
        |> List.filter (String.isEmpty >> not)
        |> String.join " "