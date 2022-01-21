import Data.List

main :: IO()
main = do
    print $ hardestSubject [("John", "Maths", 5), ("Kennedy", "English", 2), ("Joe", "Programming", 4), ("Claudia", "Programming", 6), ("Sam", "Maths", 4), ("Jenn", "English", 2)] == "English"
    print $ hardestSubject [("John", "Maths", 5), ("Kennedy", "English", 5), ("Joe", "Programming", 4), ("Claudia", "Programming", 6), ("Sam", "Maths", 4), ("Jenn", "English", 5)] == "Maths"

type Student = String
type Subject = String
type Note = Double
type Record = (Student, Subject, Note)

getSubjectsNames :: [Record] -> [Subject]
getSubjectsNames = nub . map (\ (student, subject, note) -> subject)

averageFor :: [Record] -> Subject -> Note
averageFor db subject = sum notes / (fromIntegral $ length notes)
    where
        notes = map (\ (_, _, note) -> note) $ filter (\ (_, rSubject, _) -> subject == rSubject) db

getAveragePerSubject :: [Subject] -> [Record] -> [(Subject, Note)]
getAveragePerSubject subjects db = map (\ subj -> (subj, averageFor db subj)) subjects

hardestSubject :: [Record] -> Subject
hardestSubject db = fst $ foldl1 (\ (subj1, note1) (subj2, note2) -> if note1 < note2 then (subj1, note1) else (subj2, note2)) myMap
    where myMap = getAveragePerSubject (getSubjectsNames db) db