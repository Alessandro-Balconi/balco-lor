---
layout: post
title: "Meta Report #8"
subtitle: "Patch 2.13 - Week 4 - Bandle City Waiting Room"
cover-img: /assets/img/full-art/05BC001T1-full.png
thumbnail-img: /assets/img/lor-emotes/taric.png
share-img: /assets/img/full-art/05BC001T1-full.png
tags: [meta-report]
head-extra: adsense.html
---

## Data collected

Data used in this report are collected from ranked matches of Master players in the Europe, Americas and Asia shard.

<img src="/assets/meta-report/mr8/data.png">

### Region Playrate

This chart shows the percentage of decks running a specific region.

*The sum of all Regions’ Playrate should be close to 200% since almost every deck runs 2 regions.*

![](/assets/meta-report/mr8/region_pr.png)

### Weekly Region Playrate

![image info](/assets/meta-report/mr8/region_hist.png)

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

    <img src="/assets/meta-report/mr8/champs_pr.png">

    </div>

    <div id="disp-data2" class="tabcontent">

Here you can check the full data for all champions.

<br><br>

<em>The sum of all Champions’ Playrate can be anything between 0% (if every deck is a champion-less deck) and 600% (if every deck is a 6-champions deck). Realistically, the real value will be ~200% since most decks run only 2 champions.</em>

<br><br>

   <iframe src="/assets/meta-report/mr8/champs_pr.html" 
	width="100%"
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

       <img src="/assets/meta-report/mr8/arch_pr.png">

    </div>

    <div id="disp-wr" class="tabcontent">

       <img src="/assets/meta-report/mr8/arch_wr.png">

    </div>

    <div id="disp-data" class="tabcontent">

Here you can check the full data for all archetypes.

<br><br>

   <iframe src="/assets/meta-report/mr8/arch_wr.html" 
	width="100%"
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
 
     <img src="/assets/meta-report/mr8/matchup_tbl.png">

    </div>

    <div id="disp-data3" class="tabcontent">

   <iframe src="/assets/meta-report/mr8/matchup_tbl.html" 
	width="100%"
        height="575px"
	style="border:1px solid transparent">
   </iframe>

    </div>

</div>

## Power Rankings

This chart shows the power rankings for the 10 most played archetypes.

*“Power ranking”* is defined in the *“[Vicious Syndicate](https://www.vicioussyndicate.com/) way”*; explanation taken from their website:

**Q: What is the meaning of the Power Rankings and how do you compute Power Ranking scores?**

*The Power Ranking scores are each deck’s weighted win rate against the field. We calculate a deck’s Power Ranking score by weighting its matchups against other archetypes, factoring each archetype’s frequency.*

This means that the Power ranking of a deck is not their observed winrate, but the winrate they should have according to their matchups winrate and the observed frequency of other archetypes.

*NB: if in the data collected there are no games between 2 specific archetypes, I use 50% winrate as a default value in these calculations. Since here I am filtering the most played archetypes, this should never happen; but just in case, I’m adding this as a disclaimer. Due to the small sample size, using Bayesian Averages might be a better option. I’ll think about it. In the meantime, I’m blaming Riot for the ridiculously low API rate limit.*

![](/assets/meta-report/mr8/meta_score1.png)

## Meta Score

This chart shows the position in the meta for the 10 most played archetypes, in terms of Winrate and Playrate. This one is also an exact copy of Vicious Sindycate’s Power Score chart. From the FAQs on their website:

**Q: What is the meaning of the Meta Score and how do you compute it?**

*The Meta Score is a supplementary metric that measures each archetype’s relative standing in the meta, based on both win rate and prevalence, and in comparison to the theoretical “best deck”.*

  - *We take the highest win rate recorded by a current archetype in a specific rank group, and set it to a fixed value of 100. We then determine the fixed value of 0 by deducting the highest win rate from 100%. For example, if the highest win rate recorded is 53%, a win rate of 47% will be set as the fixed value of 0. This is a deck’s Power Score. The range of 47% – 53%, whose power score ranges from 0 to 100, will contain “viable” decks. The length of this range will vary depending on the current state of the meta. Needless to say, it is possible for a deck to have a negative power score, but it can never have a power score that exceeds 100.*

  - *We take the highest frequency recorded by a current archetype in a specific rank group, and set it to a fixed value of 100. The fixed value of 0 will then always be 0% popularity. This is a deck’s Frequency Score. A deck’s frequency score cannot be a negative number.*

  - *We calculate the simple average of a deck’s Power Score and Frequency Score to find its vS Meta Score. The vS Meta Score is a deck’s relative distance to the hypothetical strongest deck in the game. Think of Power Score and Frequency Score as the coordinates (x, y) of a deck within a Scatter Plot. The Meta Score represents its relative placement in the plane between the fixed values of (0, 0) and (100,100).*

  - *If a deck records both the highest popularity and the highest win rate, its Meta Score will be 100. It will be, undoubtedly, the best deck in the game.*

![](/assets/meta-report/mr8/meta_score2.png)

### Featured Decks of the Week

In this table you can find a selection of decks piloted with great success in this week by top players in all regions.

Netdeck to your heart's content ;)

*To show up in this chart, a player needs to have played at least 30 games with a specific deck list in the last 7 days, with a winrate greater than 66%*

   <iframe src="/assets/meta-report/mr8/best_players.html" 
	width="100%"
        height="575px"   
	style="border:1px solid transparent">
   </iframe>

### Deck Codes

If you didn't find anything interesting in the previous list of decks, here are shown ALL decklists with at least 50 games played in the last week in ladder.

   <iframe src="/assets/meta-report/mr8/deck_codes.html" 
	width="100%"
        height="575px"   
	style="border:1px solid transparent">
   </iframe>

### Best Players of the Week

This table shows the players with the highest winrate in the last 7 days.

*To show up in this chart, a player needs to have played at least 50 games in the last week, with a winrate greater than 60%*

*This is kinda useless so I might remove it soon...*

   <iframe src="/assets/meta-report/mr8/player_leaderboard.html" 
	width="100%"
        height="600px"   
	style="border:1px solid transparent">
   </iframe>

<script>

document.getElementById("defaultOpen-1").click();
document.getElementById("defaultOpen-2").click();
document.getElementById("defaultOpen-3").click();

function openTab(evt, tabName, boxName) {    
    var i, tabcontent, tablinks;

    var box = document.getElementById(boxName)

    tabcontent = box.getElementsByClassName("tabcontent");
    for (i = 0; i < tabcontent.length; i++) {
        tabcontent[i].style.display = "none";
    }

    tablinks = box.getElementsByClassName("tablinks");
    for (i = 0; i < tablinks.length; i++) {
        tablinks[i].className = tablinks[i].className.replace(" active", "");
    }

    document.getElementById(tabName).style.display = "block";
    evt.currentTarget.className += " active";
}

</script>
