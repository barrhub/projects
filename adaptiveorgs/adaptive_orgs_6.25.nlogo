;; This model was coded by Shawn Barr (Redfish), Steve Hall (Lockheed Marin) and Jiang Wu (Huazhong University of Science and Technology ) 
;; during the 2008 SFI Complexity Science Summer School.  Other members of the project team (the research/theory committee) included: 
;; Catherine Spence (Intel), Bradley Jones (Nativis Inc.) and Richard Streeter (University of Edinburgh)
;; 
;; This work was partially supported by the Santa Fe Insitute whose research and education programs are
;; supported by core funding from the National Science Foundation and by gifts and grants from individuals,
;; corporations, other foundations, and members of the Institute's Business Network for Complex Systems Research.


globals [ total cumulativeBalance tickBalance saw-direction working-penetration-sites tradeCost salaries revenue i j  ]

breed [ opportunities ]
breed [ workers ]
breed [ managers ]

managers-own [ level      ;; holds the management level
              memory      ;; determines the length of % penetration list
              active?     
              my-Tmin 
              my-Tmax
              my-down-inertia
              my-up-inertia
              demand
              supply
              penetration-sites 
              memorable-penetrations 
              memorable-opportunities  
              average 
              my-workers
              my-sub-patches 
              my-manager-patch
              my-patches 
              inactive ]
              
patches-own [ acceptance? 
              worker? 
              manager? ]

to setup
  ca
  plot-pen-reset
  ask patches [ set pcolor green set worker? false set manager? false]
  setup-workers
  setup-managers-1
  set cumulativeBalance starting-balance
  set j 1
  print-stats-header
end

to go
  tick  ;; update time
  set tickBalance 0  ;; this will record how much money is made or lost this 'tick'.
  set tradeCost 0 ;; these three only used for graphing purposes
  set salaries 0
  set revenue 0 
  
  
  if layout = "south" [ update-south ]            ;; sprout the new opportunities
  if layout = "split" [ update-split ]
  if layout = "sawtooth-south" or 
     layout = "sawtooth-north" or 
     layout = "sawtooth-both" [ update-sawtooth ]
  move                                            ;; and move the 'opportunities' forward
  
  set-patch-values    ;; register the 'opportunities' coming and set the engaging worker (if any) to 'busy'
  reallocate-workers  ;; ask active managers to move the workers under their control to service the opportunities
  update-accounting   ;; ask manager to update their current state
  trade               ;; trade with your same level managers if both you and they are willing
  update-managers     ;; make sure all the manager are 'clean' ... create and remove as necessary
  update-links        ;; and make sure the links are 'proper'
  update-metrics      ;; calculates the profit and prepares the data for display on the graph
  set-update-patches  ;; recolor the map (patches/opportunities/agents and kill them off when appropriate  ... does this coorelate with score keeping? 
  print-stats
     
  ;; finally update the plot(s)
    set-current-plot-pen "cumBalance"
    plotxy ticks cumulativeBalance
    set-current-plot-pen "TickBalance"
    plotxy ticks tickBalance
    if tickComps = true
      [set-current-plot-pen "tradeCost" 
       plotxy ticks tradeCost
       set-current-plot-pen "salaries" 
       plotxy ticks salaries
       set-current-plot-pen "revenue" 
       plotxy ticks revenue
    ]
       
    
  ifelse GraphZoom > 0
    [auto-plot-off
     set-plot-y-range -500 * (21 - GraphZoom) 500 * (21 - GraphZoom)
     set-plot-x-range ticks - 50 * (21 - GraphZoom) ticks]
    [auto-plot-on]
    
    
  ;; Stop the simulation if the balance goes to $0
  ifelse Termination_Condition = "> bank balance" and cumulativeBalance <= 0 [stop]
    [ifelse Termination_Condition = "1 cycle" and ticks mod (2 * 1 / 2 ^ saw-freq * (32 + wave-breadth)) =  0 [stop]
        [ifelse Termination_Condition = "2 cycles" and ticks mod (4 * 1 / 2 ^ saw-freq * (32 + wave-breadth)) =  0 [stop]
            [ifelse Termination_Condition = "3 cycles" and ticks mod (6 * 1 / 2 ^ saw-freq * (32 + wave-breadth)) =  0 [stop]
                [ifelse Termination_Condition = "4 cycles" and ticks mod (8 * 1 / 2 ^ saw-freq * (32 + wave-breadth)) =  0 [stop]
                    [if Termination_Condition = "5 cycles" and ticks mod (10 * 1 / 2 ^ saw-freq * (32 + wave-breadth)) =  0 [stop]]]]]]
end

;--------workers---------------

to setup-workers
  ask n-of num-workers patches with [pxcor = 33] [
    sprout-workers 1 [ 
      set shape "square" 
      set size .7 
      set color blue 
      set heading 270
      if labels = true [set label who] 
    ]
  ]
end

;-------manager setup----------------

to setup-managers-1
  ask patches with [(pxcor = 37 and pycor mod 2 = 0)] [
    sprout-managers 1 [ 
      manager-attributes 
      right 2
      set level 1 
      set my-Tmax Mgr1-Tmax
      set my-Tmin Mgr1-Tmin
      set my-workers workers-on patches with [pycor = [pycor] of myself or pycor = [pycor] of myself + 1] 
      set my-manager-patch min-one-of patches with [pxcor = 41 and pycor mod 4 = 1][distance myself]
      set my-patches patches in-cone 5 25 with [pxcor = 33]
      set active? true
      create-links-with my-workers [set color blue]
      if labels = true [set label who]
    ]
  ]
end

to setup-managers [L]
  let m-level L + 1
  if m-level = 2 [
    sprout-managers 1 [
      manager-attributes 
      right 5 
      set level 2
      set my-Tmax Mgr2-Tmax
      set my-Tmin Mgr2-Tmin
      set memory memory-length 
      set my-workers workers in-cone 9 30
      set my-patches patches in-cone 9 30 with [pxcor = 33]
      set my-sub-patches patches with [pxcor = 37 and (pycor = [pycor] of myself + 1 or pycor = [pycor] of myself - 1)]
      set my-manager-patch min-one-of patches with [pxcor = 45 and pycor mod 8 = 3][distance myself]
      if labels = true [set label who]
      manager-commands
    ]
  ]
  
  if m-level = 3 [
    sprout-managers 1 [
      manager-attributes 
      right 3 
      set level 3
      set my-Tmax Mgr3-Tmax
      set my-Tmin Mgr3-Tmin
      set my-workers workers in-cone 13 38
      set my-patches patches in-cone 13 38 with [pxcor = 33]
      set my-sub-patches patches with [pxcor = 41 and (pycor = [pycor] of myself + 2 or pycor = [pycor] of myself - 2)]
      set my-manager-patch min-one-of patches with [pxcor = 49 and pycor mod 16 = 7][distance myself]
      if labels = true [set label who]
      manager-commands
    ]
  ]
  
  if m-level = 4 [
    sprout-managers 1 [
      manager-attributes 
      right 5
      set level 4
      set my-Tmax Mgr4-Tmax
      set my-Tmin Mgr4-Tmin
      set my-workers workers in-cone 18 60
      set my-patches patches in-cone 18 60 with [pxcor = 33]
      set my-sub-patches patches with [pxcor = 45 and (pycor = [pycor] of myself + 4 or pycor = [pycor] of myself - 4)]
      set my-manager-patch min-one-of patches with [pxcor = 53 and pycor mod 32 = 15][distance myself]
      if labels = true [set label who]
      manager-commands
    ]
  ]
  
  if m-level = 5 [
    sprout-managers 1 [
      manager-attributes 
      set level 5
      set my-Tmax 101
      set my-Tmin Mgr5-Tmin
      set my-workers workers with [xcor = 33]
      set my-patches patches with [pxcor = 33] 
      set my-sub-patches patches with [pxcor = 49 and (pycor = [pycor] of myself + 8 or pycor = [pycor] of myself - 8)]
      manager-commands
      if labels = true [set label who]
      ask managers with [level < 5] [set active? false]]
    ]
  
  
 ; ask managers with [active? = false and level > 1] [die]
end


to manager-attributes
  set shape "square" 
  set size .7 
  set color blue 
  set heading 270
  set memory memory-length 
  set memorable-opportunities []
  set memorable-penetrations []
end

to manager-commands
  set active? true
  ask my-sub-patches [ask managers-here [set active? false]]
  create-links-with my-workers [set color blue]
end

;---------manager update------------

to update-managers
  set j 1
  let active-flag true
  
  while [j < 6] [
    ask managers with [level = j] [
      ;; update whose working for this manager
      set my-workers workers-on my-patches
      set active-flag true    
        
      ;; set the manager's 'active' status to 'false' if she's been inactive for sufficiently long.
      if inactive > 100 and not any? workers-on my-patches ;; if no opportunities are arriving and you don't have worker then set active? to false
        [set active? false]
        
        
      ;; Consider if a higher manager should be created and do so if appropriate
      if j < 5 and average >= my-Tmax and [manager?] of my-manager-patch = false and my-up-inertia > upward-inertia and demand > 0 [
        ask my-manager-patch [
          set manager? true
          setup-managers j]
        set my-up-inertia 0 
      ]
      if level > 1 [remove-managers]
        
      ;; set the manager 'active' status to 'false if she's got workers with a higher level boss
      if active? = true and my-workers != nobody [
        ask my-workers [
           ask my-links [
             if [level] of other-end  > j [set active-flag false]
           ]
        ]
      ]
      if active-flag = false [set active? false]
 
      ;; update the graphics depending on active? status
      ifelse active? != true
        [set shape "square 2"]
        [set shape "square"]
       
    ]
    set j j + 1
  ]
end


to remove-managers
  let workerX 0  let workerY 0  
  ;; First determine if the manager needs to die
  if average < my-Tmin and length memorable-opportunities = memory-length and my-down-inertia > downward-inertia [
    ask patch-here [set manager? false ] ;; update the underlying patch with the knowledge that there's no manager here.
    ask managers-on my-sub-patches [  ;; let any immediate submanagers take over managing their workers.
      set active? true 
      create-links-with my-workers[set color blue]] ;; (re)creates links with their workers
    ask my-workers [ ;; for workers with missing mid level bosses connect them with their first level boss
      if count my-links = 1 [
        set workerX xcor  set workerY ycor
        ask managers with [level = 1] [
          ask my-patches [
            if workerX = pxcor and workerY = pycor [
              set [active?] of myself true
              ask myself [create-links-with my-workers]
            ]
          ]
        ]
      ]
    ]
    die
  ]
end

;-----------links-----------

to update-links
 ask managers [
    if level < 5 and any? managers-on my-manager-patch [ ;; if you've got an active boss you aren't in control
      set active? false
      ask my-links [die]
    ]
       
    if active? = false [ask my-links [die]]  ;; and if you're already not active you shouldn't have links.
    
    ask managers with [level < 5] [if any? my-links with [link-heading > 120 or link-heading < 60]
      [ask my-links with [link-heading > 120 or link-heading < 60][die]]]  ;; not sure if this is needed
      
    if active? = true [
      ; if not member? [[patch-here] of other-end] of my-links my-patches [die]  ; syntax??
      ask my-links [die]
      create-links-with workers-on my-patches [set color blue]
    ]
 ]
 ask workers [
   if count my-links > 1 [ask min-one-of links [link-length] [die]]
 ]
end

;-----------patches------------

to set-patch-values
  ask managers with [level = 1] [
    ifelse any? opportunities-on patch-ahead 5
      [ ask patch-ahead 4 [ set acceptance? true ]]
      [ ask patch-ahead 4 [ set acceptance? false ]]

    ifelse any? opportunities-on patch-right-and-ahead 15 5
      [ ask patch-right-and-ahead 15 4 [ set acceptance? true ]]
      [ ask patch-right-and-ahead 15 4 [ set acceptance? false ]]

    ifelse any? workers-on patch-ahead 4
      [ ask patch-ahead 4 [ set worker? true ]]
      [ ask patch-ahead 4 [ set worker? false ]]
  
    ifelse any? workers-on patch-right-and-ahead 15 4
      [ ask patch-right-and-ahead 15 4 [ set worker? true ]]
      [ ask patch-right-and-ahead 15 4 [ set worker? false ]]
  ]
end

;------------distribute workers--------------

to reallocate-workers  ;; within the span of control of a manager ... allocates one worker if available to a spot needing a worker
  ask managers with [active? = true] [
    while [any? my-workers with [acceptance? = false] and any? my-patches with [acceptance? = true and worker? = false]][
       let selected_patch one-of my-patches with [acceptance? = true and worker? = false] 
       set [ycor] of one-of my-workers with [acceptance? = false] [pycor] of selected_patch 
       ask selected_patch [set worker? true]
    ]
    ask my-workers with [acceptance? = false][set color pink]
    ask my-workers with [acceptance? = true][set color cyan]
  ]
end


to update-accounting
  ask managers [  ;; first take an accounting of where everyone is ...
    ;; check the 'average' (percent of penetrations over the specified memory interval)
    if level = 5 or [manager?] of my-manager-patch = false [ ;; if this is currently a top level manager
      ifelse average < my-Tmin
        [ set my-down-inertia my-down-inertia + 1 ]  ;; increment the down inertia if % is less the down threshold
        [ set my-down-inertia 0 ]  ;; inertia increments have to be consecutive.
      ifelse average >= my-Tmax
        [ set my-up-inertia my-up-inertia + 1 ]      ;; increment the up inertia if % is more >= the up threshold
        [ set my-up-inertia 0 ]      ;; inertia increments have to be consecutive.
    ]
    
    ;; set the worker supply and demand counts
    set demand (count my-patches with [worker? = false and acceptance? = true])     ;; note how many of worker positions are unfilled
    set penetration-sites my-patches with [worker? = false and acceptance? = true ] ;; make a list of where workers are needed
    set supply (count my-workers with [acceptance? = false])                        ;; note how many of your workers don't have jobs
  ]
end

to trade 
  ask managers [  ;; ask this of all managers
    if average >= my-Tmax  [  ;; if you've been in need over the memorable interval
      if random-float 1 < demand-inertia [ ;; determine if you're willing to ask for help (0-1). '0' represents never willing to ask.
        ask other managers with [level = [level] of myself] [  ;; only ask help of managers at the same level.  
          if random-float 1 < supply-inertia [  ;; determine if they are willing to help.  0 represents never willing to help.
            set supply count workers-on my-patches with [acceptance? = false ] ;; count up their spare workers
            if supply > 0 and any? [penetration-sites with [worker? = false]] of myself [ ;; if they've got spares and I've got current needs
              set [ycor] of one-of workers-on my-patches with [acceptance? = false ] ;; find one of their unemployed workers and ... 
                [pycor] of one-of [penetration-sites with [worker? = false]] of myself  ;; move to one of my empty penetration patches
              set supply supply - 1
              set [demand] of myself [demand] of myself - 1
              ask workers [ask patch-here [set worker? true]]                     ;; and update the patch to reflect the present worker
              set tickBalance tickBalance - trading-cost ;; and charge (the organization) for the trade
              set tradeCost tradeCost + trading-cost

            ]
          ]
        ]
      ]
    ]
  ]
end

;; Opportunity Generation  ;; Steve Hall modified here 6/23, 28
;; ===============================================================

to update-south
 ask patches with [ pxcor = 32 - startup-time and pycor < wave-breadth  ] [ sprout-opp ]
end

to update-split
 ask patches with [ pxcor = 32 - startup-time and (pycor < wave-breadth / 2 or pycor > 31 - wave-breadth / 2) ] [ sprout-opp ]
end

to update-sawtooth
  ;; Tell patches within wave-breadth of current the 'y' wave start coordinate to generate an 'opportunity'
  ask patches with [ pxcor = 32 - startup-time and (
    (pycor >= (i mod (32 + wave-breadth)) and pycor < ((wave-breadth + i) mod (32 + wave-breadth))) or
    (pycor >= ((wave-breadth + i) mod (32 + wave-breadth) - wave-breadth) and pycor < ((wave-breadth + i) mod (32 + wave-breadth))))]   
      [ sprout-opp ]
      
  ;; determine if the movement should be up, down or both
  ifelse layout = "sawtooth-north" 
    [set i i + (2 ^ saw-freq)]
    [ifelse layout = "sawtooth-south" 
      [set i i - (2 ^ saw-freq)]
      ;; double sawtooth (every 32 ticks reverse direction
      [if saw-direction = 0 [set i i - (2 ^ saw-freq)]
       if saw-direction = 1 [set i i + (2 ^ saw-freq)]
       if (ticks mod ((32 + wave-breadth) / (2 ^ saw-freq)) = 0)
         [ifelse (ticks / ((32 + wave-breadth) / (2 ^ saw-freq))) mod 2 = 0 
            [set saw-direction 0] 
            [set saw-direction 1]]]]
end

to sprout-opp
 if (ticks mod 2 = 0 and random-float 1 < even-column-prob) or
    (ticks mod 2 = 1 and random-float 1 < odd-column-prob) [
   sprout-opportunities 1 [ set size .6 set color red set shape "circle" ]
   set total total + 1
 ]
end

;; ==================================================================================================
;; Graphics Updates

to set-update-patches 
  ;; This turns opportunities 'white' if they are about to be 'engaged'.
  ask workers [
    if any? opportunities-on patch-ahead 1 [
      ask opportunities-on patch-ahead 1 [set color white] 
    ]
  ]
  ;; This turns patches 'dark blue' if they are about to be the work has been accepted and there's a worker there.       
  ask patches with [pxcor = 33][
    ifelse acceptance? = true and worker? = true [ set pcolor 102 ][ set pcolor green ]]
  ask opportunities with [ xcor = 33 and color = white ] [die] ;; Jiang WU added here 6/24 ... one tick late.
  ask opportunities with [ xcor = 33 and color = red ] [set color yellow] ;; Jiang WU added here 6/24
  ask opportunities with [ xcor = 37 and color = yellow ] [die] ;; Jiang WU added here 6/24
end

;------------metrics---------------

to update-metrics
  let managerIncome 0
  ;; Determine what the managers remember in term of missed opportunities (i.e. % of opportunities missed)
  ask managers [
    set managerIncome 0
    ;; figure out how many interceptions and penetrations occurred during this tick
    let delta-interceptions count my-patches with [pxcor = 33 and worker? = true and acceptance? = true]
    let delta-penetrations count my-patches with [pxcor = 33 and worker? = false and acceptance? = true]
    
    ;; update the total interceptions and penetrations in memory
    set memorable-penetrations fput delta-penetrations memorable-penetrations
    set memorable-opportunities fput (delta-interceptions + delta-penetrations) memorable-opportunities
    
    ;; determine that the manager doesn't remember too much
    if length memorable-opportunities > memory-length [ ;; both memorable lists are the same length
      set memorable-opportunities remove-item memory-length memorable-opportunities
      set memorable-penetrations remove-item memory-length memorable-penetrations]     
               
    ;; calculate the 'average' penetrations
    if length memorable-opportunities = memory-length [
      ifelse sum memorable-opportunities > 0
        [set average (sum memorable-penetrations / sum (memorable-opportunities) * 100)]
        [set average 0]
    ]    
    
    ;; and if anything happened calculate the tick earnings ... else start counting inactive ticks.
    ifelse delta-penetrations + delta-interceptions = 0  ;; if no opportunities are arriving here increment the inactive counter.
      [set inactive inactive + 1]
      [if active? = true [
         set managerIncome (delta-interceptions * opportunity-value - (delta-penetrations * opportunity-value * k))
         set tickBalance tickBalance + managerIncome
         set revenue revenue + managerIncome
         set inactive 0]]
  ]
 
  ;; Finally decrement the tickBalance with the required salaries to be paid this tick
  set salaries (count workers * worker-salary + 
    (count managers with [level = 1 and active? = true] * Mgr1-salary) +
    (count managers with [level = 1 and active? != true] * Mid-Mgr1) +
    (count managers with [level = 2 and active? = true] * Mgr2-salary) +
    (count managers with [level = 2 and active? != true] * Mid-Mgr2) +
    (count managers with [level = 3 and active? = true] * Mgr3-salary) +
    (count managers with [level = 3 and active? != true] * Mid-Mgr3) +
    (count managers with [level = 4 and active? = true] * Mgr4-salary) +
    (count managers with [level = 4 and active? != true] * Mid-Mgr4) +
    (count managers with [level = 5 and active? = true] * Mgr5-salary))
    
  set tickBalance (tickBalance - salaries)
  
  ;; and update the overall profit of everyone
  set cumulativeBalance (cumulativeBalance + tickBalance)

end

;-----------movement---------------

to move
 ask opportunities [
   set xcor xcor + 1]
end

;-------reporters-------------------

to print-stats-header
  let header1 "tick Mgr1-Tmin Mgr1-Tmax Mgr2-Tmin Mgr2-Tmax Mgr3-Tmin Mgr3-Tmax Mgr4-Tmin Mgr4-Tmax Mgr5-Tmin"
  let header2 "upward-inertia downward-inertia supply-inertia demand-inertia layout odd-column-prob"
  let header3 "num-workers even-column-prob opportunity-value wave-breadth memory-length saw-freq worker-visibility" 
  let header4 "starting-balance k TickBalance tradeCost salaries revenue cumBalance"
  output-print (word header1 " " header2 " " header3 " " header4)
end

to print-stats
  output-print (word ticks" "Mgr1-Tmin" "Mgr1-Tmax" "Mgr2-Tmin" "Mgr2-Tmax" "Mgr3-Tmin" "Mgr3-Tmax" "Mgr4-Tmin" "Mgr4-Tmax" "Mgr5-Tmin" "
                     upward-inertia" "downward-inertia" "supply-inertia" "demand-inertia" "layout" "odd-column-prob" "
                     num-workers" "even-column-prob" "opportunity-value" "wave-breadth" "memory-length" "saw-freq" "worker-visibility" "
                     starting-balance" "k" "TickBalance" "tradeCost" "salaries" "revenue " "cumulativeBalance)
end














@#$#@#$#@
GRAPHICS-WINDOW
573
13
1231
428
-1
-1
12.0
1
10
1
1
1
0
1
0
1
0
53
0
31
1
1
0
ticks

CC-WINDOW
5
659
1240
754
Command Center
0

BUTTON
30
62
97
95
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

CHOOSER
29
118
162
163
layout
layout
"south" "split" "sawtooth-south" "sawtooth-north" "sawtooth-both"
1

BUTTON
108
63
176
96
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL

SLIDER
751
437
873
470
worker-visibility
worker-visibility
0
10
1
1
1
NIL
HORIZONTAL

SLIDER
625
437
747
470
num-workers
num-workers
0
32
31
1
1
NIL
HORIZONTAL

SLIDER
881
437
1004
470
memory-length
memory-length
1
50
10
1
1
NIL
HORIZONTAL

SLIDER
585
473
618
623
starting-balance
starting-balance
15000
100000
25000
5000
1
$
VERTICAL

SLIDER
29
167
162
200
saw-freq
saw-freq
-2
2
-2
1
1
(2^x)
HORIZONTAL

SLIDER
175
168
307
201
wave-breadth
wave-breadth
4
32
8
4
1
NIL
HORIZONTAL

SLIDER
28
204
162
237
even-column-prob
even-column-prob
0
1.0
0.7
.05
1
NIL
HORIZONTAL

SLIDER
175
205
308
238
odd-column-prob
odd-column-prob
0
1.0
0.7
.05
1
NIL
HORIZONTAL

SLIDER
33
573
166
606
demand-inertia
demand-inertia
0
1
0
.05
1
NIL
HORIZONTAL

SLIDER
33
612
166
645
supply-inertia
supply-inertia
0
1
0
.05
1
NIL
HORIZONTAL

PLOT
625
473
1155
623
bank
days
balance
0.0
10.0
0.0
25000.0
true
true
PENS
"cumBalance" 1.0 0 -16777216 true
"tickBalance" 1.0 0 -13345367 true
"tradeCost" 1.0 0 -1184463 true
"salaries" 1.0 0 -955883 true
"revenue" 1.0 0 -10899396 true

SLIDER
1135
436
1231
469
k
k
0
1
1
.05
1
NIL
HORIZONTAL

MONITOR
197
54
283
99
survival Ticks
ticks
17
1
11

SLIDER
184
572
321
605
upward-inertia
upward-inertia
0
1000
20
1
1
NIL
HORIZONTAL

SLIDER
184
611
321
644
downward-inertia
downward-inertia
0
1000
1000
1
1
NIL
HORIZONTAL

TEXTBOX
28
291
285
318
The % of penetrations (over memory length) before a 'situation' is recognized. 
11
0.0
1

TEXTBOX
34
530
184
572
The willingness to ask peers for help or respond to their requests.
11
0.0
1

TEXTBOX
184
530
323
572
The duration a 'situation' must persist before requesting help or resigning.
11
0.0
1

TEXTBOX
176
115
326
157
Parameters that determine the structure of the 'opportunity' generation.
11
0.0
1

TEXTBOX
1020
439
1116
466
Worker Parameters
11
0.0
1

SLIDER
1158
473
1191
623
opportunity-value
opportunity-value
0
1000
200
25
1
$
VERTICAL

SLIDER
169
323
302
356
Mgr1-Tmax
Mgr1-Tmax
0
100
1.4
1
1
NIL
HORIZONTAL

SLIDER
169
358
302
391
Mgr2-Tmax
Mgr2-Tmax
0
100
2
1
1
NIL
HORIZONTAL

SLIDER
169
393
302
426
Mgr3-Tmax
Mgr3-Tmax
0
100
4
1
1
NIL
HORIZONTAL

SLIDER
168
430
301
463
Mgr4-Tmax
Mgr4-Tmax
0
100
10
1
1
NIL
HORIZONTAL

SLIDER
30
358
162
391
Mgr2-Tmin
Mgr2-Tmin
Mgr1-Tmax
Mgr1-Tmax
1.4
.1
1
NIL
HORIZONTAL

SLIDER
30
392
162
425
Mgr3-Tmin
Mgr3-Tmin
Mgr2-Tmax
Mgr2-Tmax
2
.1
1
NIL
HORIZONTAL

SLIDER
30
426
162
459
Mgr4-Tmin
Mgr4-Tmin
Mgr3-Tmax
Mgr3-Tmax
4
.1
1
NIL
HORIZONTAL

SLIDER
30
461
163
494
Mgr5-Tmin
Mgr5-Tmin
Mgr4-Tmax
Mgr4-Tmax
10
.1
1
NIL
HORIZONTAL

SLIDER
27
241
162
274
startup-time
startup-time
5
32
18
1
1
NIL
HORIZONTAL

SLIDER
447
287
567
320
worker-salary
worker-salary
0
100
1
1
1
$
HORIZONTAL

SLIDER
324
323
443
356
Mgr1-salary
Mgr1-salary
0
250
5
5
1
$
HORIZONTAL

SLIDER
448
323
567
356
Mid-Mgr1
Mid-Mgr1
0
250
2
1
1
$
HORIZONTAL

SLIDER
324
360
443
393
Mgr2-salary
Mgr2-salary
0
500
25
5
1
$
HORIZONTAL

SLIDER
448
360
567
393
Mid-Mgr2
Mid-Mgr2
0
500
10
5
1
$
HORIZONTAL

SLIDER
325
396
444
429
Mgr3-salary
Mgr3-salary
0
1000
125
5
1
$
HORIZONTAL

SLIDER
448
396
567
429
Mid-Mgr3
Mid-Mgr3
0
1000
60
10
1
$
HORIZONTAL

SLIDER
325
431
444
464
Mgr4-salary
Mgr4-salary
0
2000
600
5
1
$
HORIZONTAL

SLIDER
448
432
567
465
Mid-Mgr4
Mid-Mgr4
0
2000
300
10
1
$
HORIZONTAL

SLIDER
325
467
444
500
Mgr5-salary
Mgr5-salary
0
3000
3000
5
1
$
HORIZONTAL

TEXTBOX
360
295
418
318
Salaries
11
0.0
1

SWITCH
465
104
569
137
labels
labels
1
1
-1000

TEXTBOX
17
18
574
37
MORAS-SASOS - Modeling Robustness, Agility and Survivality of Self Adaptive Self Organizing Systems
12
0.0
1

SWITCH
438
65
571
98
survival-mode
survival-mode
1
1
-1000

SLIDER
1195
473
1228
623
trading-cost
trading-cost
0
500
250
25
1
$
VERTICAL

SLIDER
467
553
580
586
GraphZoom
GraphZoom
0
20
0
1
1
NIL
HORIZONTAL

SWITCH
467
589
580
622
TickComps
TickComps
0
1
-1000

CHOOSER
302
54
434
99
Termination_Condition
Termination_Condition
"none" "> bank balance" "1 cycle" "2 cycles" "3 cycles" "4 cycles" "5 cycles"
0

SLIDER
30
324
162
357
Mgr1-Tmin
Mgr1-Tmin
0
100
0.7
1
1
NIL
HORIZONTAL

@#$#@#$#@
WHAT IS IT?
-----------
Modeling Robustness, Agility and Survivability of Self Adaptive Self Organizing Systems (MORAS-SASOS)

MORAS-SASOS is fundamentally an implementation of the model of organizational robustness first articulated by Josh Epstein of the Brookings Institute in the final chapter of his 2007 book entitled 'Generative Social Science'.  This implementation is largely faithful to that articulation but includes a number of enhancements that provide both additional features and greater flexibility.  The interested reader will do well to reference the original document for a more detailed discussion of the origins and motivations of this model.   This particular implementation is discussed in greater detail in ' '.

MORAS-SASOS provides a design/analyis toolkit in which the policies (or traditions) governing the generation of structure and allocation of resources within a self adaptive self organizing multi-agent system of systems can be explored, modeled and/or designed.  The tool provides to the designer/analyst the ability to both model how well a particular organization's management policy will fare in one or more alternative environmental dynamics contexts or alternatively design the optimal policies for a given environmental dynamics.  

It is an assumption of this model that the robustness, agility and survivability of many 'real world' autonomous and human multi-agent systems, including military, business and cultural organizations, can be modeled within the constructs provided here ... but the validity of that assumptions largely remains for future research to establish.

Fundamentally the policies in question determine how the organization will allocate 'resources' to 'tasks' or more precisely what mix of strategies it will use.  The fundamental tradeoff is between a decentralized and market-oriented 'horizontal' strategy and a centralized authority based 'vertical' strategy.  'Optimal' strategies will emerge in many contexts that involve a fluid mix of the two strategies and it is the agent based rules that generate these optimal emergent behaviors that is at the heart of the search methods implemented in MORAS-SASOS.  

Note: there is much discussion in the literature of Complexity Science and Complex Adaptive Systems (CAS) regarding the expediency of thinking of CASs as simply another approach to solving difficult (NP Complete) optimization problems.  In one sense (properly understood) this is certainly a reasonable characterization.  In another sense however the inspiration of CASs are the self adaptive and self organzing multi-agent systems of nature that while 'struggling' for survival often and sometimes necessarily do not 'optimally' exploit the immediately available resources of their environment.  An 'optimal' survival strategy depends as much on the 'process' used to adapt to a dynamic environment then it does on a defined 'plan' that maximizes an evaluation function.  The student of Complexity Science does well to keep this distinction in mind.

In both cases the mechanism is mediated by a set of distributed agent based rules that function independent of any external mechanism ... once established.  Consequently the mechanism is fundamentally a bottom up agent based approach.

Exploring the self-organizing design space is accomplished through a self-adaptive search of the strategy 'genome' space.  This genome space is defined by parameters that fundamentally define the willingness of individual agents to trade with their peers and/or ask for 'management' support.   'Managers' are created when their help is requested but then dissolve when they perceive (as parametrically determined) that their help is no longer required.

The 'fitness' of a particular self-organizing strategy genome is determined by the 'profit' (profit = revenue - costs) of the organization as a whole.  

Costs include both the cost of maintaining the resources (note: managers may cost more than workers to maintain) and the market oriented transaction costs associated with peer level horizontal resource trades.  Costs also (optionally) include the costs of missed opportunities (the 'k' factor) that reflects the importance/repercussions of not serviceing/engaging opportunities/threats that arrive within your 'sector'.  Finally the designer/analyst can specify/model organizational 'startup' costs by varying the length of time the organization needs to be able to 'survive' on initial reserves (investment capital).  

Revenue is accumulated by successfully allocating resources to opportunities/threats.  Each successfully harvested/engaged opportunity/threat yields additional revenue to the organization.

MORAS-SASOS also provides a suite of parameters that allow the designer/analyst and/or scientist to characterize or fit the dynamics of the opportunity/threat environment.  These parameters essentially support the characterization of the 'density' of opportunities/threats as well as the rate and duration of changes in the opportunity/threat environment.


HOW IT WORKS
------------
MORAS-SASOS is intended to be used in two distinct modes: from the GUI mode and from Behavior Space mode. 

Using MORAS-SASOS in the GUI mode involves (1) adjusting the input parameters as desired, (2) pressing the 'setup' button and then (3) pressing the 'go' button.  

Generally it is expected that the parameters will be adjusted prior to starting the simualtion.  But several of the parameter settings can also be adjusted during runtime to visually experiment with their influence on emergent behavior including: the full range of parameters that influence opportunity generation, the agent salaries; the manager inertias; the 'opportunity value' and the 'k' value.  Modifying the 'threshold' values (i.e., the Tmax and Tmin) and the memory-length will influence the behavior of any subsequently created manager but will not influence existing managers.

On each 'tick' a set of opportunities/threats (red dots) are generated and begin approaching the organization.  Depending on how the parameter values are set the organization will configure itself to meet and engage those threats with its forces (light red/blue dots).  Each successful engagement (white dot) yields a positive revenue stream while missed opportunities (yellow dots) can yield negative revenue streams (depending on the 'K' value).  Each horizontal 'trade' adds a transaction cost and for each tick the salaries of the active workers/managers also produce costs.  Mid Manager (i.e. managers with bosses' can have associated reduced salaries cooresponding to their reduced responsibilty.

The threshold parameters and the inertia parameters collectively determine when and how managers are 'hired' and 'surplussed' and when resources are simply traded in the market place.

It is highly recommended that the new user experiment with all these parameters to get a sense of what how they influence the consequent behavior of the collective.


Using MORAS-SASOS in the Behavior Space mode is initiated by selecting the Netlogo 'Behavior Space' tool found under the Netlogo 'Tools' submenu.  The Behavior Space tool facilitates the specification and execution of experiments and its use is desribed in the NetLogo User Manual within the 'Features' section.  Using the Behavior Space tool allow both the space of organization genomes to be systematically explored as well as the space of environmental dynamics.



HOW TO USE IT
-------------
The Section briefly describes the function of each GUI control:
	
Top Level Controls :

1.  Setup : 
Uses the current parameter settings to 'randomly' create the initial configuration of the model.

2.  Go :
Begins execution of the model and continues until pressed again ... or the model terminates.

3.  Ticks
Displays the number of time intervals (days) the simulation has advanced.

4.  Termination Condition
The simulation has three basic termination conditions selectable via the 'Termination_Condition' chooser.  The first option is 'none' under which the simulation runs until the 'go' button is pressed again.  The second option is '> bank balance' under which conditions it will run until the organization's bank balance is less than or equal to $0.  The third option is to run the specified number of cycles or periods of the opportunity generation pattern.  The user can selected 1-5 cycles.

5.  Labels
Turns on/off the agent labels. Particularly useful for model development.


Opportunity Generation : 

1.  Layout

Provides five different options for specifying the dynamics of the opportunity/threat stream generation.

2.  Saw-Freq

Determines the frequency of the sawtooth stream dynamics.

3.  Wave-Breadth

Determines the breadth (& depth) of the opportunity stream.

4.  Even-Column-Probability

Determine the probability that an opportunity will be generated within each defined location within the specified opportunity stream for even numbered stream columns.

5.  Odd-Column-Probabiity

Determine the probability that an opportunity will be generated within each defined location within the specified opportunity stream for odd numbered stream columns.

6.  Startup Time

Determines how far the opportunities are generated in front of the organization.  Thus it influences required 'startup costs' of the organization.


Worker Parameters : 

1.  Worker Visibility

Currently unutilized.

2.  Number of Workers

Determines the number of 'workers' / 'teams' that can be deployed to engage the opportunies.  If the maximum number (32) is specified then all opportunities will be engaged.

Manager Parameters : 

1.  Memory Length

Determines how long (how many 'ticks') a manager can remember what's happened in past.

2.  Tmin

Determines the maximum percentage (%) of missed opportunities (averaged over the specified memory-length) that will trigger Downward Inertia to grow.  

3.  Tmax

Determines the minimum percentage (%) of missed opportunities (averaged over the specified memory-length) that will trigger Upward Inertia to grow.

4.  Worker/Manager Salaries

Determines what the active workers and managers will be paid each tick.

5.  MidManager Salaries

Determines what a mid-level manager (one with a boss) is paid.


Coordination Parameters : 

1.  Demand Inertia

The willingness to ask for help from your trading partners.  Ranges from 0 (never willing) to 1 (always willing). 

2.  Supply Inertia

The willingness to provide help to your trading partners.  Ranges from 0 (never willing) to 1 (always willing). 

3.  Upward Inertia

Determines how many consecutive 'ticks' the maximum Threshold (Tmax) must be met or exceeded before the manager starts trading and/or asking for management support. 

4.  Downward Inertia

Determines how many consecutive 'ticks' the minimum Threshold (Tmin) must fail to be meet before the manager must more surplus itself).

Accounting Parameters : 

1.  Starting Balance

The starting balance determines how much 'money' is in the bank at the start of the simulation.  Interacts with start-up time and salaries to determine initial viability.

2.  Opportunity Value

Determines the value of the revenue collected each time an opportunity is successfully harvested.

3.  Trading Cost

Determines the tranaction costs of each successfully completed trade amongst peer level managers.

4.  'K' value

Determines what fraction of the opportunity's Opportunity Value is considered a missed opportunity and accounted as a organizational cost.  Provides a means of encoding the value of 'dominanting the market' as opposed to maximizing profit.


THINGS TO NOTICE
----------------
The Map Display - Upper Panel

The 'Map Display' graphically displays the behavior of the organizational genome.  The color coding is described below :

Red Circles 	= Opportunities (threats) that are potentially 'engagable' or 'serviceable'
White Circles 	= Opportunities that are being successfully engaged.
Yellow Circles	= Opportunites (penetrations) that have not been engaged/serviced.
Lt Blue Circles	= Workers that are profitably engaged
Lt Red Circles 	= Workers that are not profitably engaged
Blue Squares	= Active Managers (draw full salaries)
Black Squares	= Inactive Managers (draw mid-level salaries)
Blue Lines	= Workers (left node) actively taskable by the manager (right node)

The Graph Display - Lower Panel

The 'Graph Display' charts both the total organizational profit per tick (the 'tickProfit') in Blue and the cumulative organization profit (the 'cumProfit') in Black.  If the 'survival mode' toggle is set to 'on' then the simulation will terminate when the cumulative profit is less than or equal to zero.

The three component of the tickProfit can also be viewed by setting the 'TickComps' switch to 'on'.  When this switch is on the tick, revenue will be displayed along with the trade and salary costs.  

The Graph Display supports a simple zoom capability that is particularly useful in examining the behavior of the tick-by-tick behavior of the organization.  Access this capability by setting the GraphZoom slider to a non-zero number.  Larger numbers provide more zoom.


THINGS TO TRY
-------------

Generally experimenting with the various parameters is a good way to get a feel for  the sensitivity of the organization's agility, survivability and robustness to such genomic mutations and should help define the space to be explored for an 'optimal' genome.  

Pay particular attention to the 'K' factor.  The 'K' factor is thought to generally influence the degree to which dynamic hybrid strategies evolve.


EXTENDING THE MODEL
-------------------

While this is a powerful model of organization adaptability, agility and survivability in itself, in no small part because of its abstract simplicity, there are many directions in which the research might continue and the model developed.  Joshua Epstein mentions a number of these in his original articulation of the model and the interested reader is encouraged again to reference that paper.  

Many other developments can also be readily envisioned including:

1.  Explicit modeling of the costs associated with management recruiting/dismissal.
2.  Reactive (Adaptive Organizational) generation of the Opportunities/Threats
3.  Mapping procedures from existing/proposed organizational policies to/from the model abstractions.
4.  Allowing workers to be hired and fired based on the bank balance.


NETLOGO FEATURES
----------------
This model translates succussfully in Netlogo 3D.  That might be useful if extensions of the model lead to the desire to represent 2D opportunity fields.


RELATED MODELS
--------------
Joshua Epstein's team has implemented a similar model in ASPECT 


CREDITS AND REFERENCES
----------------------
Joshua M. Epstein,  Generative Social Science : Studies in Agent-Based Computational Modeling.  (Princeton, New Jersey.  Princeton University Press, 2007).

This work was partially supported by the Santa Fe Insitute whose research and education programs are supported by core funding from the National Science Foundation and by gifts and grants from individuals, corporations, other foundations, and members of the Institute's Business Network for Complex Systems Research.
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

grass patch
false
0
Rectangle -10899396 true false 0 0 300 300
Rectangle -16777216 true false 0 285 300 300
Rectangle -16777216 true false 285 0 300 330
Rectangle -1 true false 0 0 285 15
Rectangle -1 true false 0 0 15 285

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

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 4.0.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="genome-1" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="533"/>
    <metric>cumulativeBalance</metric>
    <enumeratedValueSet variable="Mgr1-Tmin">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr1-Tmax">
      <value value="1.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr2-Tmin">
      <value value="1.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr2-Tmax">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr3-Tmin">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr3-Tmax">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr4-Tmin">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr4-Tmax">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr5-Tmin">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="upward-inertia">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="downward-inertia">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="supply-inertia">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="demand-inertia">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;south&quot;"/>
      <value value="&quot;split&quot;"/>
      <value value="&quot;sawtooth-both&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="odd-column-prob">
      <value value="0.3"/>
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-workers">
      <value value="2"/>
      <value value="3"/>
      <value value="5"/>
      <value value="7"/>
      <value value="11"/>
      <value value="13"/>
      <value value="15"/>
      <value value="18"/>
      <value value="21"/>
      <value value="25"/>
      <value value="31"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="even-column-prob">
      <value value="0.7"/>
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="opportunity-value">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wave-breadth">
      <value value="8"/>
      <value value="12"/>
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-length">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="saw-freq">
      <value value="-2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="worker-visibility">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-balance">
      <value value="25000"/>
      <value value="50000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="genome-3" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="533"/>
    <metric>cumulativeBalance</metric>
    <enumeratedValueSet variable="Mgr1-Tmin">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr1-Tmax">
      <value value="1.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr2-Tmin">
      <value value="1.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr2-Tmax">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr3-Tmin">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr3-Tmax">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr4-Tmin">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr4-Tmax">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr5-Tmin">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="upward-inertia">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="downward-inertia">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="supply-inertia">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="demand-inertia">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;split&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="odd-column-prob">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-workers">
      <value value="2"/>
      <value value="3"/>
      <value value="5"/>
      <value value="11"/>
      <value value="31"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="even-column-prob">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="opportunity-value">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wave-breadth">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-length">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="saw-freq">
      <value value="-2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="worker-visibility">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-balance">
      <value value="25000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="genome-4" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="533"/>
    <metric>cumulativeBalance</metric>
    <enumeratedValueSet variable="Mgr1-Tmin">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr1-Tmax">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr2-Tmin">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr2-Tmax">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr3-Tmin">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr3-Tmax">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr4-Tmin">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr4-Tmax">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr5-Tmin">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="upward-inertia">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="downward-inertia">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="supply-inertia">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="demand-inertia">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;split&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="odd-column-prob">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-workers">
      <value value="2"/>
      <value value="3"/>
      <value value="5"/>
      <value value="11"/>
      <value value="31"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="even-column-prob">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="opportunity-value">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wave-breadth">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-length">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="saw-freq">
      <value value="-2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="worker-visibility">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-balance">
      <value value="25000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="genome-5" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="533"/>
    <metric>cumulativeBalance</metric>
    <enumeratedValueSet variable="Mgr1-Tmin">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr1-Tmax">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr2-Tmin">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr2-Tmax">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr3-Tmin">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr3-Tmax">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr4-Tmin">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr4-Tmax">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr5-Tmin">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="upward-inertia">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="downward-inertia">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="supply-inertia">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="demand-inertia">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;sawtooth-both&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="odd-column-prob">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-workers">
      <value value="2"/>
      <value value="3"/>
      <value value="5"/>
      <value value="11"/>
      <value value="31"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="even-column-prob">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="opportunity-value">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wave-breadth">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-length">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="saw-freq">
      <value value="-2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="worker-visibility">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-balance">
      <value value="25000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="genome-6" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="533"/>
    <metric>cumulativeBalance</metric>
    <enumeratedValueSet variable="Mgr1-Tmin">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr1-Tmax">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr2-Tmin">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr2-Tmax">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr3-Tmin">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr3-Tmax">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr4-Tmin">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr4-Tmax">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr5-Tmin">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="upward-inertia">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="downward-inertia">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="supply-inertia">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="demand-inertia">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;sawtooth-both&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="odd-column-prob">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-workers">
      <value value="2"/>
      <value value="3"/>
      <value value="5"/>
      <value value="11"/>
      <value value="31"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="even-column-prob">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="opportunity-value">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wave-breadth">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-length">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="saw-freq">
      <value value="-2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="worker-visibility">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-balance">
      <value value="25000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="genome-2" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="533"/>
    <metric>cumulativeBalance</metric>
    <enumeratedValueSet variable="Mgr1-Tmin">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr1-Tmax">
      <value value="1.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr2-Tmin">
      <value value="1.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr2-Tmax">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr3-Tmin">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr3-Tmax">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr4-Tmin">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr4-Tmax">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr5-Tmin">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="upward-inertia">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="downward-inertia">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="supply-inertia">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="demand-inertia">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;south&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="odd-column-prob">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-workers">
      <value value="2"/>
      <value value="3"/>
      <value value="5"/>
      <value value="11"/>
      <value value="31"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="even-column-prob">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="opportunity-value">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wave-breadth">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-length">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="saw-freq">
      <value value="-2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="worker-visibility">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-balance">
      <value value="25000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="genome-7" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="533"/>
    <metric>cumulativeBalance</metric>
    <enumeratedValueSet variable="Mgr1-Tmin">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr1-Tmax">
      <value value="1.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr2-Tmin">
      <value value="1.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr2-Tmax">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr3-Tmin">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr3-Tmax">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr4-Tmin">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr4-Tmax">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr5-Tmin">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="upward-inertia">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="downward-inertia">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="supply-inertia">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="demand-inertia">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;sawtooth-both&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="odd-column-prob">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-workers">
      <value value="2"/>
      <value value="3"/>
      <value value="5"/>
      <value value="11"/>
      <value value="31"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="even-column-prob">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="opportunity-value">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wave-breadth">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-length">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="saw-freq">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-balance">
      <value value="30000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="genome-8" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="533"/>
    <metric>cumulativeBalance</metric>
    <enumeratedValueSet variable="Mgr1-Tmin">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr1-Tmax">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr2-Tmin">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr2-Tmax">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr3-Tmin">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr3-Tmax">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr4-Tmin">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr4-Tmax">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr5-Tmin">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="upward-inertia">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="downward-inertia">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="supply-inertia">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="demand-inertia">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;sawtooth-both&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="odd-column-prob">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-workers">
      <value value="2"/>
      <value value="3"/>
      <value value="5"/>
      <value value="11"/>
      <value value="31"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="even-column-prob">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="opportunity-value">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wave-breadth">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-length">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="saw-freq">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-balance">
      <value value="30000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="genome-9" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="533"/>
    <metric>cumulativeBalance</metric>
    <enumeratedValueSet variable="Mgr1-Tmin">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr1-Tmax">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr2-Tmin">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr2-Tmax">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr3-Tmin">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr3-Tmax">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr4-Tmin">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr4-Tmax">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr5-Tmin">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="upward-inertia">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="downward-inertia">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="supply-inertia">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="demand-inertia">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;sawtooth-both&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="odd-column-prob">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-workers">
      <value value="2"/>
      <value value="3"/>
      <value value="5"/>
      <value value="11"/>
      <value value="31"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="even-column-prob">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="opportunity-value">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wave-breadth">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-length">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="saw-freq">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-balance">
      <value value="30000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="genome-10" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="533"/>
    <metric>cumulativeBalance</metric>
    <enumeratedValueSet variable="Mgr1-Tmin">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr1-Tmax">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr2-Tmin">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr2-Tmax">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr3-Tmin">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr3-Tmax">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr4-Tmin">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr4-Tmax">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr5-Tmin">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="upward-inertia">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="downward-inertia">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="supply-inertia">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="demand-inertia">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;sawtooth-both&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="odd-column-prob">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-workers">
      <value value="2"/>
      <value value="3"/>
      <value value="5"/>
      <value value="11"/>
      <value value="31"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="even-column-prob">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="opportunity-value">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wave-breadth">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-length">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="saw-freq">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-balance">
      <value value="30000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="genome-11" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="533"/>
    <metric>cumulativeBalance</metric>
    <enumeratedValueSet variable="Mgr1-Tmin">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr1-Tmax">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr2-Tmin">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr2-Tmax">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr3-Tmin">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr3-Tmax">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr4-Tmin">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr4-Tmax">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr5-Tmin">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="upward-inertia">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="downward-inertia">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="supply-inertia">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="demand-inertia">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;sawtooth-both&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="odd-column-prob">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-workers">
      <value value="2"/>
      <value value="3"/>
      <value value="5"/>
      <value value="11"/>
      <value value="31"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="even-column-prob">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="opportunity-value">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wave-breadth">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-length">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="saw-freq">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-balance">
      <value value="30000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="genome-12" repetitions="50" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="533"/>
    <metric>cumulativeBalance</metric>
    <enumeratedValueSet variable="Mgr1-Tmin">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr1-Tmax">
      <value value="1.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr2-Tmin">
      <value value="1.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr2-Tmax">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr3-Tmin">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr3-Tmax">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr4-Tmin">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr4-Tmax">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mgr5-Tmin">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="upward-inertia">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="downward-inertia">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="supply-inertia">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="demand-inertia">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout">
      <value value="&quot;sawtooth-both&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="odd-column-prob">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-workers">
      <value value="2"/>
      <value value="3"/>
      <value value="5"/>
      <value value="11"/>
      <value value="31"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="even-column-prob">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="opportunity-value">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wave-breadth">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="memory-length">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="saw-freq">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-balance">
      <value value="30000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="k">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
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
