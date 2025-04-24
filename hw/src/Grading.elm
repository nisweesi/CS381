-- USAGE:
--
-- Program files are in subdirectory src/
-- Start Elm with "elm repl"
-- Load program with "import Grading exposing (..)"

module Grading exposing (..)

type alias Points = Int

type Grade = Pass | Fail

alice : Points
alice = 65

grade : Points -> Grade
grade p = if p>50 then Pass else Fail
