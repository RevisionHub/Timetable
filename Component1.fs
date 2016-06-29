namespace Timetable
type SpecificTime = {day:int;part:int}
type Tweakables = 
    {
        daysBetween : int
        partsPerDay : int
        timeOff : SpecificTime[]
        breaks : int[]
        minimumFraction : int
        maximumFraction : int
        acceptableOffset : int
    }
type Subject = 
    {
        subject : string
        difficultyPos:int
        //likePos:int
    }
type Routine =
    |TimeOff = 0
    |Break = 1
    |Work = 2
type TimetableRequest = {days : int;subjects:Subject[];tweakables:Tweakables; routine:(Routine*int)[]}
[<AutoOpen>]
module Core = 
    let t a b = (a*60)+b
    //the int in routine is minutes
    let createTimetable(t:TimetableRequest) = 
        let conf = t.tweakables
        let routine = t.routine
        let parts = 
            Array.init (t.days) (fun (i) -> 
                Array.mapi (fun j (r,t) -> 
                    let t' = {day=i;part=j}
                    Choice2Of2(if Array.contains t' conf.timeOff then Routine.TimeOff else r),t
                ) routine
            )
        let totalTime = Array.sumBy(Array.sumBy<_,int>(function|Choice2Of2(Routine.Work),i -> i|_ -> 0)) parts
        //now let's form the timetable
        let nodes = t.subjects|>Array.sortBy(fun (i:Subject) -> i.difficultyPos)
        let max = float conf.maximumFraction
        let min = float conf.minimumFraction
        let step = (max-min)/float (nodes.Length-1)
        let v = [|min..step..max|]
        let all = Array.sum v
        let times = Array.map (fun i -> i/all * float totalTime) v
        printfn "%A" times
        //Now we have worked out the perfect amount of time for each one, we can fit them in
        let rec fit (subject:Subject) (slots:(Choice<string,Routine>*int)[][]) (left:float) =
            if left <= float conf.acceptableOffset then slots else
            let s = Seq.indexed slots
            let i = 
                Seq.tryPick(fun (i,e) ->
                    Array.tryFindIndex(
                        function 
                        |Choice2Of2(Routine.Work),i -> abs(int(left)-i)<conf.acceptableOffset
                        |_->false
                    ) e
                    |> Option.map (fun j -> i,j)
                ) s
            match i with 
            |Some(a,b) -> 
                let c = Array.copy slots
                let v = snd c.[a].[b]
                c.[a].[b] <- Choice1Of2(subject.subject),v
                c
            |None ->
                //We are going to have to split!
                let j = 
                    Seq.tryPick(fun (i,e) ->
                        Array.tryFindIndex(
                            function 
                            |Choice2Of2(Routine.Work),i -> int(left)>i
                            |_->false
                        ) e
                        |> Option.map (fun j -> i,j)
                    ) s
                match j with
                |Some(a,b) ->
                    let c = Array.copy slots
                    let v = snd c.[a].[b]
                    c.[a].[b] <- Choice1Of2(subject.subject),v
                    fit subject c (left-(float v))
                |None ->
                    //We have run out of good slots!!!
                    failwith "I need to fix this!"
        Array.fold2(fun acc time subject -> fit subject acc time) parts times t.subjects