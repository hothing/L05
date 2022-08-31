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

    let saturation vertice aColoredGraph =
        Set.count (adjacentColors vertice aColoredGraph)
    

    
