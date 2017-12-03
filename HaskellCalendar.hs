import qualified Data.Time.Clock as Clock
import Data.Time.Format (formatTime, defaultTimeLocale)
import System.Environment (getArgs)

readTimetable :: FilePath -> IO (Bool, String, [((String, String), (String, String), Int)])
readTimetable file =
    do contents <- readFile file
       let ls = lines contents
           (week:location:_, _:rs) = break (== "") ls
           (ts, _:lessons) = break (== "") rs
           times = map (\x -> let (start:end:_) = words x
                              in (start,end)) ts
       return $ processLessons week location lessons times
    where processLessons week location lessons times =
              (words week !! 1 == "A", location,
               zip3 (map (\x -> let (name,room) = lessonSplit x
                                in (name, room)) (filter (not . null) lessons))
                        (concat $ repeat times)
                        (concatMap (replicate 8) [0,1,2,3,4,7,8,9,10,11]))
          lessonSplit (s:ss) =
              case s of
                ' ' -> ("",ss)
                '\\' -> let (name,room) = lessonSplit (tail ss)
                        in (' ':name,room)
                _   -> let (name,room) = lessonSplit ss
                       in (s:name,room)
          lessonSplit _      = ("","")

createEvent :: String -> String -> String -> String -> String -> Clock.UTCTime -> Int -> Int -> String
createEvent name room location start end startMondayDate dayOffset event_id =
    let startTime = format $ Clock.addUTCTime (timeToDiff start dayOffset) startMondayDate
        endTime = format $ Clock.addUTCTime (timeToDiff end dayOffset) startMondayDate
        date = format startMondayDate
        day = days !! (if dayOffset < 5 then dayOffset else dayOffset - 7)
    in "BEGIN:VEVENT\nDTSTART;TZID=Europe/London:" ++ startTime ++ "\nDTEND;TZID=Europe/London:" ++ endTime ++ "\nRRULE:FREQ=WEEKLY;INTERVAL=2;BYDAY=" ++ day ++ "\nDTSTAMP:" ++ date ++ "\nUID:" ++ show event_id ++ date ++ "thjread@gmail.com\nCREATED:" ++ date ++ "\nDESCRIPTION:" ++ room ++ "\nLAST-MODIFIED:" ++ date ++ "\nLOCATION:" ++ location ++ "\nSEQUENCE:0\nSTATUS:CONFIRMED\nSUMMARY:" ++ name ++ " (" ++ room ++ ")" ++ "\nTRANSP:OPAQUE\nEND:VEVENT\n"
    where timeToDiff :: String -> Int -> Clock.NominalDiffTime
          timeToDiff time dayOffset =
              let hours = read $ take 2 time
                  minutes = read $ drop 3 time
              in fromIntegral . toInteger $ (((hours+24*dayOffset)*60)+minutes)*60
          format :: Clock.UTCTime -> String
          format = formatTime defaultTimeLocale "%Y%m%dT%H%M%S"
          days = ["MO", "TU", "WE", "TH", "FR", "SA", "SU"]

createTimetable :: [((String, String), (String, String), Int)] -> String -> Clock.UTCTime -> String
createTimetable timetable location startMondayDate =
    preamble ++ concatMap (\(((name, room), (start, end), dayOffset), event_id) -> createEvent name room location start end startMondayDate dayOffset event_id) (zip timetable [0..]) ++ end
        where preamble = "BEGIN:VCALENDAR\nPRODID:HaskellTimetable\nVERSION:2.0\nCALSCALE:GREGORIAN\nMETHOD:PUBLISH\nX-WR-CALNAME:Timetable\nX-WR-TIMEZONE:Europe/London\nX-WR-CALDESC:\nBEGIN:VTIMEZONE\nTZID:Europe/London\nX-LIC-LOCATION:Europe/London\nBEGIN:DAYLIGHT\nTZOFFSETFROM:+0000\nTZOFFSETTO:+0100\nTZNAME:BST\nDTSTART:19700329T010000\nRRULE:FREQ=YEARLY;BYMONTH=3;BYDAY=-1SU\nEND:DAYLIGHT\nBEGIN:STANDARD\nTZOFFSETFROM:+0100\nTZOFFSETTO:+0000\nTZNAME:GMT\nDTSTART:19701025T020000\nRRULE:FREQ=YEARLY;BYMONTH=10;BYDAY=-1SU\nEND:STANDARD\nEND:VTIMEZONE\n"
              end = "END:VCALENDAR\n"

lastMondayA :: Bool -> Clock.UTCTime -> Clock.UTCTime
lastMondayA isA date =
    let dayStart = Clock.UTCTime (Clock.utctDay date) 0
        weekDay = read (formatTime defaultTimeLocale "%u" dayStart) - 1
        offset = fromIntegral . toInteger $
                 -24*60*60*(weekDay + if not isA then 7 else 0)
    in Clock.addUTCTime offset dayStart
    --in if weekDay == 0 then dayStart
    --   else let offset = fromIntegral . toInteger $ 24*60*60*(7-weekDay) :: Clock.NominalDiffTime
    --        in Clock.addUTCTime offset dayStart

makeTimetable :: FilePath -> FilePath -> IO ()
makeTimetable fileIn fileOut =
    do currentDate <- Clock.getCurrentTime
       (week, location, timetableData) <- readTimetable fileIn
       let timetableString = createTimetable timetableData location
                             (lastMondayA week currentDate)
       writeFile fileOut timetableString

main :: IO ()
main = do args <- getArgs
          makeTimetable (args !! 0) (args !! 1)
