globals [
  new-grade          ;; new average grade
  previous-grade     ;; average of all grades in previous round

  clustering-coefficient               ;; the clustering coefficient of the network; this is the
                                       ;; average of clustering coefficients of all turtles
  average-path-length                  ;; average path length of the network
  infinity                             ;; a very large number.
                                         ;; used to denote distance between two turtles which
                                         ;; don't have a connected or unconnected path between them
  highlight-string                     ;; message that appears on the node properties monitor
  estd                                 ;; standard deviation of expectations
]

turtles-own [
  node-clustering-coefficient   ;; clustering coefficient of node
  distance-from-other-turtles   ;; list of distances of this node from other turtles
  grade         ;; students' grades
  grade-last-tick ;; storing last tick
  expectation   ;; educational expectation in years
  expectation-last-tick ;; storing last tick for plot
  pexpe ;; peer adaptation
  sexpe ;; self adaptation
  change ;; count update
  ]

links-own
[
  rewired?                    ;; keeps track of whether the link has been rewired or not
]

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

to setup ;;para configurar
 __clear-all-and-reset-ticks ;;limpiar todo
 ask patches [set pcolor white]
 set-default-shape turtles "person"
 make-students

 ;;; network constructed using NetLogo Small World model
 set infinity 99999  ;; just an arbitrary choice for a large number
 if soc-influence = true [wire-all] ;; if the switch is activated, connect the class
 ask links [set color grey]

end

to make-students
   create-turtles Num-stud  ;; number of students
   layout-circle (sort turtles) max-pxcor - 1  ;; arrange them in a circle in order by who number
   ask turtles [ ;; agents begin with randomly initialized representation weights
   initialize-variables;; initialize grades with normal distribution
   choose-color ;; choose expectation-color
   ]
end

to initialize-variables
   set grade random 4 ;; random starting grade, greater or equal to 0, but less 4 (exclusive). Alternative random-normal 2 1 with average 2 and sd 1
   set expectation random 7 ;; random educational expectation from 9 (0) years to 15 (6)
   set estd standard-deviation [expectation] of turtles
end


to go
  tick
  ask turtles [ ;; every turn, agents perform, update their expectation
    update; agents bayesian learning
    choose-color ;; update the educational level color
    ;; Social influence from Shaw 2015
    if soc-influence = false [
       set expectation  (precision (expectation + sexpe) 1) ] ;;
    if soc-influence = true [update-net
      set expectation (precision (expectation + sexpe + pexpe) 1) ] ;; from social influence
    if expectation > 6 [ ;; expectations grow linearly ad infinitum. We might define a ceiling.
       set expectation 6]
    if expectation < 0 [ ;; expectations grow linearly ad infinitum. We might define a floor.
       set expectation 0]
    set estd standard-deviation [expectation] of turtles

  if expectation > expectation-last-tick [ set change 1] ;; upward
  if expectation < expectation-last-tick [ set change 2] ;; downward
  if expectation = expectation-last-tick [ set change 0] ;; stable
  ]
end

to update-net ;; agent adapt their expectation considering the expectation of neighbors
;set expectation expectation + (link-neighbors expectation)
  let gr0 ((count link-neighbors with [grade = 0]) * 0) ; count num of neigh
  let gr1 ((count link-neighbors with [grade = 1]) * 1)
  let gr2 ((count link-neighbors with [grade = 2]) * 2)
  let gr3 ((count link-neighbors with [grade = 3]) * 3)
  let neigh (count link-neighbors)
  if  neigh > 0 [ ;; only for those who have neigh
  let gr-neigh ((gr1 + gr2 + gr3) / neigh) ; average epectation of the neighbors
  if  direction = "contagion" [
  if gr-neigh > grade [
    set pexpe adaptation * (gr-neigh - grade) ; adaptation by average exp of neighbors
  ]
  if gr-neigh <= grade [
   set pexpe adaptation * (-1 * (gr-neigh - grade)) ; adaptation by average exp of neighbors
  ]
    ]
  if direction = "fish-pond" [
  if gr-neigh > grade [
    set pexpe adaptation * (-1 * (gr-neigh - grade)) ; adaptation by average exp of neighbors
  ]
  if gr-neigh <= grade [
   set pexpe adaptation * (gr-neigh - grade) ; adaptation by average exp of neighbors
  ]
    ]
    ]
end

to update
let rgrade random 4;; random part of the new grade
set new-grade (grade * path-dependance) + (rgrade * (1 - path-dependance));; new-grade conditional on previous grade
set expectation-last-tick expectation ;; store before updating
set sexpe (new-grade - grade) * adaptation ;; expectation as a function of the grade difference and a rate of adaptation
set grade-last-tick grade             ;; store before updating
set grade new-grade ; new-grade becomes the grade of the new tick, otherwise, it will be always the initialised
;; precision rounds numbers to 1 decimal places
end

to choose-color
  ;; Color according to the expected years for each educational level;; I included the whole set of decimals otherwise upward update are understimated
  if expectation = 0 [set color blue]
  if expectation = 0.1 [set color blue]
  if expectation = 0.2 [set color blue]
  if expectation = 0.3 [set color blue]
  if expectation = 0.4 [set color blue]
  if expectation = 0.5 [set color blue]
  if expectation = 0.6 [set color blue]
  if expectation = 0.7 [set color blue]
  if expectation = 0.8 [set color blue]
  if expectation = 0.9 [set color blue]
  if expectation = 1   [set color red]
  if expectation = 1.1 [set color red]
  if expectation = 1.2 [set color red]
  if expectation = 1.3 [set color red]
  if expectation = 1.4 [set color red]
  if expectation = 1.5 [set color red]
  if expectation = 1.6 [set color red]
  if expectation = 1.7 [set color red]
  if expectation = 1.8 [set color red]
  if expectation = 1.9 [set color red]
  if expectation = 2   [set color red]
  if expectation = 2.1 [set color red]
  if expectation = 2.2 [set color red]
  if expectation = 2.3 [set color red]
  if expectation = 2.4 [set color red]
  if expectation = 2.5 [set color red]
  if expectation = 2.6 [set color red]
  if expectation = 2.7 [set color red]
  if expectation = 2.8 [set color red]
  if expectation = 2.9 [set color red]
  if expectation = 3 [set color red]
  if expectation = 3.1 [set color red]
  if expectation = 3.2 [set color red]
  if expectation = 3.3 [set color red]
  if expectation = 3.4 [set color red]
  if expectation = 3.5 [set color red]
  if expectation = 3.6 [set color red]
  if expectation = 3.7 [set color red]
  if expectation = 3.8 [set color red]
  if expectation = 3.9 [set color red]
  if expectation = 4 [set color green]
  if expectation = 4.1 [set color green]
  if expectation = 4.2 [set color green]
  if expectation = 4.3 [set color green]
  if expectation = 4.4 [set color green]
  if expectation = 4.5 [set color green]
  if expectation = 4.6 [set color green]
  if expectation = 4.7 [set color green]
  if expectation = 4.8 [set color green]
  if expectation = 4.9 [set color green]
  if expectation = 5 [set color green]
  if expectation = 5.1 [set color green]
  if expectation = 5.2 [set color green]
  if expectation = 5.3 [set color green]
  if expectation = 5.4 [set color green]
  if expectation = 5.5 [set color green]
  if expectation = 5.6 [set color green]
  if expectation = 5.7 [set color green]
  if expectation = 5.8 [set color green]
  if expectation = 5.9 [set color green]
  if expectation = 6 [set color green]
end

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Procedures used to construct networks according to NetLogo Small Worlds implementation of
;;; Strogatz-Watts Small World algorithm. See Info tab for citation.
;;;;;;;;;;;;;;;;;;;;;;;;

to wire-all

  ;; make sure num-turtles is setup correctly; if not run setup first
  if count turtles != Num-stud [
    setup
  ]

  ;; set up a variable to see if the network is connected
  let success? false

  ;; if we end up with a disconnected network, we keep trying, because the APL distance
  ;; isn't meaningful for a disconnected network.
  while [not success?] [
    ;; kill the old lattice, reset neighbors, and create new lattice
    ask links [ die ]
    wire-them

    ask links [

      ;; whether to rewire it or not?
      if (random-float 1) < rewiring-prob
      [
        ;; "a" remains the same
        let node1 end1
        ;; if "a" is not connected to everybody
        if [ count link-neighbors ] of end1 < (count turtles - 1)
        [
          ;; find a node distinct from node1 and not already a neighbor of node1
          let node2 one-of turtles with [ (self != node1) and (not link-neighbor? node1) ]
          ;; wire the new edge
          ask node1 [ create-link-with node2 [set rewired? true ] ]


          set rewired? true
        ]
      ]
      ;; remove the old edge
      if (rewired?)
      [
        die
      ]
    ]

    ;; check to see if the new network is connected and calculate path length and clustering
    ;; coefficient at the same time
    set success? do-calculations
  ]

end

;; do-calculations reports true if the network is connected,
;;   and reports false if the network is disconnected.

to-report do-calculations

  ;; set up a variable so we can report if the network is disconnected
  let connected? true

  ;; find the path lengths in the network
  find-path-lengths

  let num-connected-pairs sum [length remove infinity (remove 0 distance-from-other-turtles)] of turtles

  ;; In a connected network on N nodes, we should have N(N-1) measurements of distances between pairs,
  ;; and none of those distances should be infinity.
  ;; If there were any "infinity" length paths between nodes, then the network is disconnected.
  ;; In that case, calculating the average-path-length doesn't really make sense.
  ifelse ( num-connected-pairs != (count turtles * (count turtles - 1) ))
  [
      set average-path-length infinity
      ;; report that the network is not connected
      set connected? false
  ]
  [
    set average-path-length (sum [sum distance-from-other-turtles] of turtles) / (num-connected-pairs)
  ]
  ;; find the clustering coefficient and add to the aggregate for all iterations
  find-clustering-coefficient

  ;; report whether the network is connected or not
  report connected?
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Clustering computations ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report in-neighborhood? [ hood ]
  report ( member? end1 hood and member? end2 hood )
end


to find-clustering-coefficient
  ifelse all? turtles [count link-neighbors <= 1]
  [
    ;; it is undefined
    ;; what should this be?
    set clustering-coefficient 0
  ]
  [
    let total 0
    ask turtles with [ count link-neighbors <= 1]
      [ set node-clustering-coefficient "undefined" ]
    ask turtles with [ count link-neighbors > 1]
    [
      let hood link-neighbors
      set node-clustering-coefficient (2 * count links with [ in-neighborhood? hood ] /
                                         ((count hood) * (count hood - 1)) )
      ;; find the sum for the value at turtles
      set total total + node-clustering-coefficient
    ]
    ;; take the average
    set clustering-coefficient total / count turtles with [count link-neighbors > 1]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Path length computations ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Implements the Floyd Warshall algorithm for All Pairs Shortest Paths

to find-path-lengths
  ;; reset the distance list
  ask turtles
  [
    set distance-from-other-turtles []
  ]

  let i 0
  let j 0
  let k 0
  let node1 one-of turtles
  let node2 one-of turtles
  let node-count count turtles
  ;; initialize the distance lists
  while [i < node-count]
  [
    set j 0
    while [j < node-count]
    [
      set node1 turtle i
      set node2 turtle j
      ;; zero from a node to itself
      ifelse i = j
      [
        ask node1 [
          set distance-from-other-turtles lput 0 distance-from-other-turtles
        ]
      ]
      [
        ;; 1 from a node to it's neighbor
        ifelse [ link-neighbor? node1 ] of node2
        [
          ask node1 [
            set distance-from-other-turtles lput 1 distance-from-other-turtles
          ]
        ]
        ;; infinite to everyone else
        [
          ask node1 [
            set distance-from-other-turtles lput infinity distance-from-other-turtles
          ]
        ]
      ]
      set j j + 1
    ]
    set i i + 1
  ]
  set i 0
  set j 0
  let dummy 0
  while [k < node-count]
  [
    set i 0
    while [i < node-count]
    [
      set j 0
      while [j < node-count]
      [
        ;; alternate path length through kth node
        set dummy ( (item k [distance-from-other-turtles] of turtle i) +
                    (item j [distance-from-other-turtles] of turtle k))
        ;; is the alternate path shorter?
        if dummy < (item j [distance-from-other-turtles] of turtle i)
        [
          ask turtle i [
            set distance-from-other-turtles replace-item j distance-from-other-turtles dummy
          ]
        ]
        set j j + 1
      ]
      set i i + 1
    ]
    set k k + 1
  ]

end

;;;;;;;;;;;;;;;;;;;;;;;
;;; Edge Operations ;;;
;;;;;;;;;;;;;;;;;;;;;;;

;; creates a new lattice
to wire-them
  ;; iterate over the turtles
  let n 0
  while [n < count turtles]
  [
    ;; make edges with the next two neighbors
    ;; this makes a lattice with average degree of 4
    make-edge turtle n
              turtle ((n + 1) mod count turtles)
    make-edge turtle n
              turtle ((n + 2) mod count turtles)
    set n n + 1
  ]
end

;; connects the two turtles
to make-edge [node1 node2]
  ask node1 [ create-link-with node2  [
    set rewired? false
  ] ]
end
@#$#@#$#@
GRAPHICS-WINDOW
295
10
732
448
-1
-1
13.0
1
10
1
1
1
0
0
0
1
-16
16
-16
16
0
0
1
ticks
30.0

BUTTON
119
80
183
113
Setup
Setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
74
123
145
156
Go
Go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
13
224
143
257
adaptation
adaptation
0
3
3.0
0.1
1
NIL
HORIZONTAL

BUTTON
157
122
234
155
Go once
go 
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
18
38
148
71
Num-stud
Num-stud
10
70
70.0
1
1
NIL
HORIZONTAL

PLOT
748
10
948
160
Expectation
Time
% of students
0.0
10.0
0.0
100.0
true
true
"" ""
PENS
"DO" 1.0 0 -13345367 true "" "plot (count turtles with [color = blue]) / (count turtles) * 100"
"HS" 1.0 0 -2674135 true "" "plot (count turtles with [color = red]) / (count turtles) * 100"
"CD" 1.0 0 -14439633 true "" "plot (count turtles with [color = green]) / (count turtles) * 100"

SWITCH
12
266
143
299
soc-influence
soc-influence
0
1
-1000

SWITCH
155
266
286
299
segregation
segregation
1
1
-1000

MONITOR
17
171
145
216
NIL
clustering-coefficient
3
1
11

MONITOR
156
171
284
216
NIL
average-path-length
3
1
11

SLIDER
157
38
287
71
rewiring-prob
rewiring-prob
0
1
0.39
0.01
1
NIL
HORIZONTAL

SLIDER
156
224
286
257
path-dependance
path-dependance
0
1
0.23
.01
1
NIL
HORIZONTAL

TEXTBOX
754
421
943
463
© Francisco Olivos\nThe Chinese University of Hong Kong
11
0.0
1

CHOOSER
12
308
143
353
direction
direction
"contagion" "fish-pond"
0

MONITOR
748
168
867
213
Standard Deviation
estd
3
1
11

PLOT
748
237
948
387
Updating
NIL
NIL
0.0
10.0
0.0
100.0
true
true
"" ""
PENS
"U" 1.0 0 -13345367 true "" "plot (count turtles with [change = 1]) / (count turtles) * 100"
"D" 1.0 0 -2674135 true "" "plot (count turtles with [change = 2]) / (count turtles) * 100"
"S" 1.0 0 -7500403 true "" "plot (count turtles with [change = 0]) / (count turtles) * 100"

@#$#@#$#@
## WHAT IS IT?

This model tests whether students adapt educational expectations when considering social influence

## HOW IT WORKS

Each agent has three traits: a) educational-expectation, b) old-grade, c) new-grade

At each time step, the following events occur:

1. Agents start with a random grade and educational expectation. 

2. Each time the agent will recive a new-grade and update the educational-expectation.

3. The adaptation-rate could vary.  



## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

1. You can vary the size of the class from 10 students to 70 and the rate in which students respond to their new-grade (adaptation). 


## EXTENDING THE MODEL

1. Allow path dependance of grades
2. Allow social influence
3. Incorporate educational segregation

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.1.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
