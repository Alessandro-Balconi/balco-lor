---
layout: post
title: "Meta Report #%1$s"
subtitle: "%2$s"
cover-img: /assets/img/full-art/%3$s-full.png
thumbnail-img: /assets/img/lor-emotes/%4$s.png
share-img: /assets/img/full-art/%3$s-full.png
tags: [meta-report]
head-extra: adsense.html
---

## Data collected

Data used in this report are collected from ranked matches of Master players in the Europe, Americas and Asia shard.

<img src="/assets/meta-report/mr%1$s/data.png">

### Region Playrate

This chart shows the percentage of decks running a specific region.

*The sum of all Regions’ Playrate should be close to 200%% since almost every deck runs 2 regions.*

![](/assets/meta-report/mr%1$s/region_pr.png)

### Weekly Region Playrate

![image info](/assets/meta-report/mr%1$s/region_hist.png)

### Champions Playrate

<div id="code-box-2">
    <div class="tab">
        <button class="tablinks" onclick="openTab(event, 'disp-chart2','code-box-2')" id="defaultOpen-2">Chart</button>
        <button class="tablinks" onclick="openTab(event, 'disp-data2','code-box-2')">Full Data</button>
        <div class="spacer"></div>
    </div>

    <div id="disp-chart2" class="tabcontent">

This chart shows the percentage of decks running a specific champion.

<br><br>

    <img src="/assets/meta-report/mr%1$s/champs_pr.png">

    </div>

    <div id="disp-data2" class="tabcontent">

Here you can check the full data for all champions.

<br><br>

<em>The sum of all Champions’ Playrate can be anything between 0%% (if every deck is a champion-less deck) and 600%% (if every deck is a 6-champions deck). Realistically, the real value will be ~200%% since most decks run only 2 champions.</em>

<br><br>

   <iframe src="/assets/meta-report/mr%1$s/champs_pr.html" 
	width="100%%"
        height="575px"   
	style="border:1px solid transparent">
   </iframe>

    </div>

</div>

### Archetypes

*At the moment, Archetypes are defined as combinations of champions and regions.*

<div id="code-box-1">
    <div class="tab">
        <button class="tablinks" onclick="openTab(event, 'disp-pr','code-box-1')" id="defaultOpen-1">Playrate</button> &nbsp;
        <button class="tablinks" onclick="openTab(event, 'disp-wr','code-box-1')" id="defaultOpen-1">Winrate</button> &nbsp;
        <button class="tablinks" onclick="openTab(event, 'disp-data','code-box-1')">Full Data</button>
        <div class="spacer"></div>
    </div>

    <div id="disp-pr" class="tabcontent">

       <img src="/assets/meta-report/mr%1$s/arch_pr.png">

    </div>

    <div id="disp-wr" class="tabcontent">

       <img src="/assets/meta-report/mr%1$s/arch_wr.png">

    </div>

    <div id="disp-data" class="tabcontent">

Here you can check the full data for all archetypes.

<br><br>

   <iframe src="/assets/meta-report/mr%1$s/arch_wr.html" 
	width="100%%"
        height="575px"   
	style="border:1px solid transparent">
   </iframe>

    </div>

</div>

### Matchup Table

<div id="code-box-3">
    <div class="tab">
        <button class="tablinks" onclick="openTab(event, 'disp-chart3','code-box-3')" id="defaultOpen-3">Chart</button>
        <button class="tablinks" onclick="openTab(event, 'disp-data3','code-box-3')">Full Data</button>
        <div class="spacer"></div>
    </div>

    <div id="disp-chart3" class="tabcontent">

     The value shown in each cell is the winrate that the Archetype in the row has when playing against the Archetype in the column.
 
     <img src="/assets/meta-report/mr%1$s/matchup_tbl.png">

    </div>

    <div id="disp-data3" class="tabcontent">

   <iframe src="/assets/meta-report/mr%1$s/matchup_tbl.html" 
	width="100%%"
        height="575px"
	style="border:1px solid transparent">
   </iframe>

    </div>

</div>

