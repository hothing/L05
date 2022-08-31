module ColorAlgebra 
    
    let makeColorsSet colorList =
        let allColors = Set colorList
        let et = (List.head colorList)
        let sc = Set.singleton et 
        (allColors, Set.remove et sc)

    let includeColor color colorSet =
        let ac, sc = colorSet
        if Set.contains color ac then
            (ac, Set.add color sc)
        else
            invalidArg (nameof color) (sprintf "Undedined color %A" color)

    let excludeColor color colorSet =
        let ac, sc = colorSet
        if Set.contains color ac then
            (ac, Set.remove color sc)
        else
            invalidArg (nameof color) (sprintf "Undedined color %A" color)

    let containsColor color colorSet =
        let ac, sc = colorSet
        Set.contains color sc

    let unionColors colorSet1 colorSet2 =
        let ac1, sc1 = colorSet1
        let ac2, sc2 = colorSet2
        if ac1 = ac2 then
            (ac1, Set.union sc1 sc2)
        else
            invalidArg (nameof colorSet2) (sprintf "The universum sets are different")

    let differenceColors colorSet1 colorSet2 =
        let ac1, sc1 = colorSet1
        let ac2, sc2 = colorSet2
        if ac1 = ac2 then
            (ac1, Set.difference sc1 sc2)
        else
            invalidArg (nameof colorSet2) (sprintf "The universum sets are different")
