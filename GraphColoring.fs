module GraphColoring 

    open ALGraph

    let makeColoredGraph nonColor aGraph  =
        let colorMap = Map.map (fun k v -> nonColor) aGraph
        (colorMap, aGraph)

    let saturation nonColor vertice aColoredGraph =
        let colorMap, aGraph = aColoredGraph
        let adjVx = adjacents vertice aGraph
        let colors = Set.map (fun v -> Map.find v colorMap) adjVx
        Set.count (Set.filter (fun c -> c <> nonColor) colors)

    let setColor aColor aVrtice aColoredGraph =
        let colorMap, aGraph = aColoredGraph
        let nColormap = Map.change aVrtice (fun x -> match x with Some(color) -> Some(aColor) | None -> None) colorMap
        (nColormap, aGraph)

    let color aVrtice aColoredGraph =
        let colorMap, aGraph = aColoredGraph
        Map.find aVrtice colorMap

    let color2 aVrtice aColoredGraph =
        let colorMap, aGraph = aColoredGraph
        Map.tryFind aVrtice colorMap

    