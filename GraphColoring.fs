module GraphColoring 

    open ALGraph

    type NodeAttribute = int option * int

    let makeColoredGraph aGraph  =
        let colorMap = Map.map (fun k v -> NodeAttribute(None, 0)) aGraph
        (aGraph, colorMap)

    let getColorMap aColoredGraph =
        let _, aColorMap = aColoredGraph
        aColorMap

    let setColor  aVertice aColor aColorMap =
        let setColorAttribute attrs =
            let _, aWeight = attrs
            (Some(aColor), aWeight)     
        Map.change aVertice (fun x -> match x with Some(a) -> Some(setColorAttribute a) | None -> None) aColorMap
        
    let unsetColor aVertice aColorMap =
        let setColorAttribute attrs =
            let _, aWeight = attrs
            (None, aWeight)     
        Map.change aVertice (fun x -> match x with Some(a) -> Some(setColorAttribute a) | None -> None) aColorMap
        
    let getColor aVertice aColorMap =
        let aColor, _ = Map.find aVertice aColorMap
        aColor

    let getAttributes aVertice aColoredGraph =
        let _, aColorMap = aColoredGraph
        Map.tryFind aVertice aColorMap

    let increaseWeight aVertice aColorMap =
        let setAttribute attrs =
            let aColor, aWeight = attrs
            (aColor, aWeight + 1)     
        Map.change aVertice (fun x -> match x with Some(a) -> Some(setAttribute a) | None -> None) aColorMap

    let resetWeight aVertice aColorMap =
        let setAttribute attrs =
            let aColor, _ = attrs
            (aColor, 0)     
        Map.change aVertice (fun x -> match x with Some(a) -> Some(setAttribute a) | None -> None) aColorMap
    
    let adjacentColors aVertice aGraph aColorMap =
        let adjVx = adjacents aVertice aGraph
        let attrs = Set.map (fun v -> Map.find v aColorMap) adjVx
        let asx = Set.filter (fun ax -> (fst ax) <> None) attrs
        Set.map (fun ax -> fst ax) asx |> Set.toList |> List.choose id |> Set.ofList
    
    let calculateWeights aGraph aColorMap =
        Map.map (fun vertice attrs -> 
            let aColor, _ = attrs
            (aColor, Set.count (adjacentColors vertice aGraph aColorMap))) aColorMap
    
    let rec doColorization allColors aGraph aColorMap =
        let verticies =   
                Map.filter (fun v a -> (fst a) = None) aColorMap
                |> Map.toList 
                
        //printfn "[DBG][doColorization] verticies %A on colormap %A" verticies aColorMap

        if not (List.isEmpty verticies) then
            let highestSat = 
                verticies
                |> List.map (fun x -> (snd x |> snd)) 
                |> List.max
            let selVertice = 
                verticies 
                |> List.filter (fun x -> (snd x |> snd) >= highestSat) 
                |> List.map (fun x -> fst x)
                |> List.head // here we should add ramdom selection
            let adjColors = adjacentColors selVertice aGraph aColorMap
            let availableColors = Set.difference allColors adjColors
            let newColor , allColors = 
                if Set.isEmpty availableColors then
                    let hColor = (Set.maxElement allColors) + 1
                    // printfn "[DBG][doColorization] new color"
                    (hColor, Set.add hColor allColors)
                else
                    (Set.minElement availableColors, allColors)
            //printfn "[DBG][doColorization] selected color %A" newColor
            let aColorMap = setColor selVertice newColor aColorMap |> calculateWeights aGraph
            doColorization allColors aGraph aColorMap
        else 
            aColorMap

    let coloring aGraph =
        let aGraph, aColorMap = makeColoredGraph aGraph
        doColorization (Set [1]) aGraph aColorMap |> Map.map (fun v a -> (fst a))

    let chormaticNumberByColorMap aColorMap =
        let x = Map.toList aColorMap |> List.map (fun a -> (snd a))
        x |> List.choose id |> List.max

    let chormaticNumber aGraph =
        let aColorMap = coloring aGraph
        let x = Map.toList aColorMap |> List.map (fun a -> (snd a))
        x |> List.choose id |> List.max
