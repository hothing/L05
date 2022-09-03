module GraphColoring 

    open ALGraph
   
    let nonColor = 0

    let makeColoredGraph countColors aGraph  =
        let colorMap = Map.map (fun k v -> nonColor) aGraph
        (aGraph, colorMap, Set [for c in nonColor + 1 .. countColors -> c])

    let isValidColor aColor allColors =
        Set.contains aColor allColors

    let getColorMap aColoredGraph =
        let aGraph, aColorMap, allColors = aColoredGraph
        aColorMap

    let changeColorMap aColorMap aColoredGraph  =
        let aGraph, _, allColors = aColoredGraph
        if Map.forall (fun k v -> (isValidColor v allColors) || (v = nonColor)) aColorMap then
            (aGraph, aColorMap, allColors)
        else
            invalidArg (nameof aColorMap) "A colormap has invalid color(s)"

    let setColor aColor aVertice aColoredGraph =
        let _, colorMap, allColors = aColoredGraph
        if isValidColor aColor allColors then
            let nColormap = Map.change aVertice (fun x -> match x with Some(_) -> Some(aColor) | None -> None) colorMap
            changeColorMap nColormap aColoredGraph
        else
            invalidArg (nameof aColor) (sprintf "A color is invalid -> %A" aColor) 

    let unsetColor aVertice aColoredGraph =
        let nColormap = Map.change aVertice (fun x -> match x with Some(_) -> Some(nonColor) | None -> None) (getColorMap aColoredGraph)
        changeColorMap nColormap aColoredGraph

    let color aVertice aColoredGraph =
        Map.find aVertice (getColorMap aColoredGraph)

    let color2 aVertice aColoredGraph =
        Map.tryFind aVertice (getColorMap aColoredGraph)

    let adjacentColors aVertice aColoredGraph =
        let aGraph, aColorMap, _  = aColoredGraph
        let adjVx = adjacents aVertice aGraph
        let colors = Set.map (fun v -> Map.find v aColorMap) adjVx
        (Set.filter (fun c -> c <> nonColor) colors)

    let adjacentColorsExt aVertice aColoredGraph =
        let aGraph, aColorMap, allColors  = aColoredGraph
        let adjVx = adjacents aVertice aGraph
        let colors = Set.map (fun v -> Map.find v aColorMap) adjVx
        let theColors = (Set.filter (fun c -> c <> nonColor) colors)
        (theColors, Set.difference allColors theColors)

    let adjacentColors0 aVertice aColorMap aGraph =
        let adjVx = adjacents aVertice aGraph
        let colors = Set.map (fun v -> Map.find v aColorMap) adjVx
        (Set.filter (fun c -> c <> nonColor) colors)

    let rec doColorization allColors aColorMap aGraph =
        let verticies = Map.keys (Map.filter (fun v c -> c = nonColor) aColorMap) |> List.ofSeq
        //printfn "[DBG][doColorization] vertices %A" verticies
        if not (List.isEmpty verticies) then
            let getEnv v =
                let vx = adjacentColors0 v aColorMap aGraph
                let availableColors = Set.difference allColors vx
                //printfn "[DBG][doColorization] adjacents %A with %A" vx availableColors
                let nextColor = if Set.isEmpty availableColors // WARNING! Design problem: how to handle situation when there is no any color available
                                    then Set.minElement allColors // WARNING! Can trigger invalid result
                                    else Set.minElement availableColors 
                (Set.count vx, nextColor)
            let adjMat = Map (List.map (fun v -> (v, getEnv v)) verticies)
            //printfn "[DBG][doColorization] sudoku %A" adjMat
            let highestSat = Map.values adjMat |> Seq.toList |> List.map (fun x -> (fst x)) |> List.max
            //printfn "[DBG][doColorization] highest saturation level = %A" highestSat
            let selVertice = Map.pick (fun v env -> if (fst env) >= highestSat then Some(v) else None) adjMat
            //printfn "[DBG][doColorization] selected vertice %A" selVertice
            let _, newColor = Map.find selVertice adjMat
            //printfn "[DBG][doColorization] selected color %A"  newColor
            let nColorMap = Map.change selVertice (fun v -> match v with Some(_) -> Some(newColor) | None -> None) aColorMap
            //printfn "[DBG][doColorization] new color map %A" nColorMap
            doColorization allColors nColorMap aGraph
        else
            //printfn "[DBG][doColorization] finish"
            aColorMap 

    let coloring reset aColoredGraph =
        let aGraph, aColorMap, allColors = aColoredGraph
        let nColorMap = if reset then Map.map (fun k v -> nonColor) aGraph else aColorMap
        doColorization allColors nColorMap aGraph

