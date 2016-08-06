import ElmTest exposing (..)
import Model exposing (..)


assertModOrder : List String -> List (String, List String) -> Assertion
assertModOrder expected deps =
  assertEqual expected (fst <| Model.init deps).groupedModNames


assertCount : Int -> Int -> List (String, List String) -> Assertion
assertCount modCount dirCount deps =
  let
    model = fst <| Model.init deps
  in
    assertEqual
      (modCount, dirCount)
      (List.length model.groupedModNames, List.length model.dirNames)


orderTests : Test
orderTests =
  suite "Order"
    [ test "basic" ( assertModOrder [] [] )
    , test "basic" ( assertModOrder ["A"] [("A", [])] )
    , test "basic" ( assertModOrder ["A", "B"] [("A", ["B"]), ("B", [])] )
    , test "basic" ( assertModOrder ["A", "B"] [("B", []), ("A", ["B"])] )
    , test "basic" ( assertModOrder ["A", "B", "C"] [("C", []), ("B", ["C"]), ("A", ["B"])] )
    , test "basic" ( assertModOrder ["A", "B", "C"] [("B", ["C"]), ("C", []), ("A", ["B"])] )
    , test "basic" ( assertModOrder ["A", "B", "C"] [("C", []), ("B", ["C"]), ("A", ["B", "C"])] )
    , test "basic" ( assertModOrder ["A", "B", "C"] [("B", ["C"]), ("C", []), ("A", ["B", "C"])] )
    , test "directory" ( assertModOrder ["D.A"] [("D.A", [])] )
    , test "directory" ( assertModOrder ["D.A", "D.B"] [("D.A", ["D.B"]), ("D.B", [])] )
    , test "directory" ( assertModOrder ["D.A", "D.B"] [("D.B", []), ("D.A", ["D.B"])] )
    , test "directory" ( assertModOrder ["D1.A", "D2.B"] [("D1.A", ["D2.B"]), ("D2.B", [])] )
    , test "directory" ( assertModOrder ["D1.A", "D2.B"] [("D2.B", []), ("D1.A", ["D2.B"])] )
    , test "circuler" ( assertModOrder ["D1.A", "D1.C", "D2.B"] [("D1.A", ["D2.B"]), ("D2.B", ["D1.C"]), ("D1.C", [])] )
    , test "circuler" ( assertModOrder ["D1.A", "D1.C", "D2.B"] [("D1.A", ["D2.B", "D1.C"]), ("D2.B", ["D1.C"]), ("D1.C", [])] )
    , test "circuler" ( assertModOrder ["D1.A", "D1.C", "D2.B"] [("D2.B", ["D1.C"]), ("D1.A", ["D2.B"]), ("D1.C", [])] )
    , test "circuler" ( assertModOrder ["D1.A", "D1.C", "D2.B"] [("D1.C", []), ("D1.A", ["D2.B", "D1.C"]), ("D2.B", ["D1.C"])] )
    , test "circuler" ( assertModOrder ["D1.A", "D1.D", "D2.B", "D2.C"] [("D1.A", ["D2.B"]), ("D2.B", ["D2.C"]), ("D2.C", ["D1.D"]), ("D1.D", [])] )
    , test "circuler" ( assertModOrder ["D1.A", "D1.D", "D2.B", "D2.C"] [("D2.C", ["D1.D"]), ("D1.D", []), ("D1.A", ["D2.B"]), ("D2.B", ["D2.C"])] )
    , test "circuler" ( assertModOrder ["D1.A", "D1.D", "D2.B", "D2.C"] [("D1.A", ["D2.B"]), ("D2.B", ["D2.C", "D1.D"]), ("D2.C", []), ("D1.D", [])] )
    , test "circuler" ( assertModOrder ["D1.A", "D1.D", "D2.B", "D2.C"] [("D1.A", ["D2.B", "D2.C"]), ("D2.B", ["D2.C"]), ("D2.C", ["D1.D"]), ("D1.D", [])] )
    , test "circuler" ( assertModOrder ["D1.A", "D1.D", "D2.B", "D2.C"] [("D1.A", ["D2.B", "D2.C", "D1.D"]), ("D2.B", ["D2.C"]), ("D2.C", []), ("D1.D", ["D2.C"])] )
    ]


countTests : Test
countTests =
  suite "Count"
    [ test "basic" ( assertCount 0 0 [] )
    , test "basic" ( assertCount 1 1 [("A", [])] )
    , test "basic" ( assertCount 1 1 [("A", ["Lib"])] )
    , test "basic" ( assertCount 2 1 [("A", ["B", "Lib"]), ("B", ["Lib"])] )
    , test "basic" ( assertCount 2 1 [("A", ["B", "Lib1"]), ("B", ["Lib2"])] )
    , test "basic" ( assertCount 1 1 [("D.A", [])] )
    , test "basic" ( assertCount 1 1 [("D.A", ["Lib"])] )
    , test "basic" ( assertCount 2 1 [("D.A", []), ("D.B", [])] )
    , test "basic" ( assertCount 2 1 [("D.A", ["D.B", "Lib"]), ("D.B", ["Lib"])] )
    , test "basic" ( assertCount 2 1 [("D.A", ["D.B", "L.Lib1"]), ("D.B", ["D.Lib2"])] )
    , test "basic" ( assertCount 2 2 [("D1.A", ["D2.B", "Lib"]), ("D2.B", ["Lib"])] )
    , test "basic" ( assertCount 2 2 [("D1.A", ["D2.B", "L.Lib1"]), ("D2.B", ["D1.Lib2"])] )
    ]


tests : Test
tests =
  suite "DepCheck"
    [ orderTests
    , countTests
    ]


main : Program Never
main =
  runSuite tests
