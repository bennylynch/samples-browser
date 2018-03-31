module Program


open Fable.Core
open Fable.Import.Browser
open System.Threading

type Complex = { r : double; i : double }
  with member x.Norm = (x.r ** 2.) + (x.i ** 2.) |> sqrt

type Color = { r : int; g : int; b : int; a : int }

let maxIter = 255

let height = 512
let width = 512

let minX = -2.1
let maxX = 2.1
let minY = -2.1
let maxY = 2.1

let r = document.getElementById("r") :?> HTMLInputElement
let i = document.getElementById("i") :?> HTMLInputElement
let stop = document.getElementById("stop") :?> HTMLInputElement

let mutable c = {r = r.valueAsNumber; i = i.valueAsNumber }
    
let iteratePoint  (p : Complex) : Complex =
    //{ r = s.r + p.r*p.r - p.i*p.i; i = s.i + 2.0 * p.i * p.r }
    {r = (p.r * p.r) - (p.i * p.i) + c.r; i = (p.r * p.i) + (p.r * p.i) + c.i }

let getIterationCount (p : Complex) =
    let mutable z = p// {r = 0.; i = 0.}//p
    let mutable i = 0
    while i < maxIter && (z.Norm < 2.0) do
      z <- iteratePoint z
      i <- i + 1
    i

let getCoordColor (x : int, y : int) : Color =
    let p = { r = float x * (maxX - minX) / float width + minX
            ; i = float y * (maxY - minY) / float height + minY }
    let i = getIterationCount p

    match i with
    |_ when i > 0 && i < 64 ->  { r = i; g = 0; b = 0; a = 255}
    |_ when i > 64 && i < 128 -> { r = 0; g = i; b = 0; a = 255}
    |_ when i > 128 && i < 192 -> { r = 0; g = 0; b = i; a = 255}
    |_ -> { r = i ; g = i /2; b = i /4; a = 255}

    //{ r = i; g = i; b = i; a = 255}



let showSet() =
    let ctx = document.getElementsByTagName_canvas().[0].getContext_2d()

    let img = ctx.createImageData(U2.Case1 (float width), float height)
    
    for y = 0 to height-1 do
        for x = 0 to width-1 do
            let index = (x + y * width) * 4
            let color = getCoordColor (x, y)
            img.data.[index+0] <- float color.r
            img.data.[index+1] <- float color.g
            img.data.[index+2] <- float color.b
            img.data.[index+3] <- float color.a
    ctx.putImageData(img, 0., 0.)

let cts = ref (new CancellationTokenSource())

let rec loop iteration = async {
  //vary c r and i +/- 50%, over ranage of 100 iterations.
  //if iteration % 100 <= 50, then -; else +

  let r' = r.valueAsNumber
  let i' = i.valueAsNumber
  match iteration % 100 with
  |a when a < 50 -> c <- { r = r' - (float a /100. * r');
                           i = (float a / 100.) + i'   }
  |a             ->  c <- { r = r' + (float a /100. * r')
                            i = i' - (float a / 100.)}
  showSet()
  do! Async.Sleep 10
  return! loop (iteration + 1)                       
}

//r.addEventListener_change(fun _ -> showSet(); box())
//i.addEventListener_change(fun _ -> showSet(); box())
//showSet()
let mutable stopped = false
stop.addEventListener_click (fun _ -> 
                  match stopped with 
                  |true -> 
                      stop.textContent <- "stop"
                      cts := new CancellationTokenSource()
                      Async.StartImmediate (loop 0, (!cts).Token)
                  |false  -> 
                    stop.textContent <- "start"
                    (!cts).Cancel()
                  //printfn "%A" stopped
                  stopped <- not stopped
                  box() )
Async.StartImmediate (loop 0, (!cts).Token)