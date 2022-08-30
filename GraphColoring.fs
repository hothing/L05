module GraphColoring 

    open ALGraph

    let colorsAlgebra nonColor colorsCount =
        let allColors = Set [ for i in (nonColor + 1) .. colorsCount -> i ]
        let validColor color =
            Set.contains color allColors
        let includeColor color colorsSet =
            if validColor color then
                Set.add color colorsSet
            else
                colorsSet
        let excludeColor color colorsSet = Set.remove color colorsSet
        let complementColors = Set.difference allColors
        (validColor, includeColor, excludeColor, complementColors)

    let makeColoredGraph nonColor aGraph  =
        let colorMap = Map.map (fun k v -> nonColor) aGraph
        (colorMap, aGraph)

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

    let adjacentColors validColor vertice aColoredGraph =
        let colorMap, aGraph = aColoredGraph
        let adjVx = adjacents vertice aGraph
        let colors = Set.map (fun v -> Map.find v colorMap) adjVx
        (Set.filter (fun c -> not (validColor c)) colors)
    
    let saturation validColor vertice aColoredGraph =
        Set.count (adjacentColors validColor vertice aColoredGraph)
    

    
