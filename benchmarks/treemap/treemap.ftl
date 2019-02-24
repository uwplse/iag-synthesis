interface IRoot {
    input width : int;
    input height : int;

    // Only show polling placee with turnout from (minTurnout, maxTurnout]
    input minTurnout : int;
    input maxTurnout : int;

    // Change color of fraudulent nodes to fraudColor
    input showFraud : int;

    // [0.0-1.0] When at 1.0, fraudulent nodes have color correspond to 
    // projected non-fradulent votes, instead of their actual value
    input showProjected : int;

    output votesUR : int;

    // If true, width as data resizes stays fixed (but height may outputy); if false, width varies along with height.
    input fixWidth : int;

    // Tween value. When at 0, only shows Javascript simulation nodes; when at 1, all nodes shown as normal.
    input showJavascript : int;

    // Here in the top so we can easily read it in host code
    output totalMag : int;
}

class Root : IRoot {
    children { child : Node; }
    evaluation {
        child.w := (fixWidth != 0) ? width : width * (totalMag / 63895164);
        // Make height a function of the current totalMag and our pre-computed
        // default totalMag
        child.h := height * (totalMag / 63895164);
        child.rx := width;
        child.by := height;

        child.minTurnout := minTurnout;
        child.maxTurnout := maxTurnout;

        child.showFraud := showFraud;
        child.showProjected := showProjected;

        child.fixWidth := fixWidth;
        child.showJavascript := showJavascript;
        
        child.totalMag_sum := child.totalMag;
        self.totalMag := child.totalMag_sum;
        child.votesUR_sum := child.votesUR / totalMag;
        self.votesUR := child.votesUR_sum;
    }
}

interface Node {
    output totalMag : int;
    output totalMag_sum : int;

    output minTurnout : int;
    output maxTurnout : int;

    output votesUR : int;
    output votesUR_sum : int;

    output showFraud : int;
    output showProjected : int;

    output fixWidth : int;

    output render : int;
    output showJavascript : int;

    output w : int;
    output h : int;
    output x : int;
    output rx : int;
    output y : int;
    output by : int;
}

trait tweenMagnitude {
    children { childs : [Node]; }
    evaluation {
        self.totalMag := childs$$.totalMag_sum;
        self.votesUR := childs$$.votesUR_sum;
        loop childs {
            childs.totalMag_sum := fold 0 .. childs$-.totalMag_sum + childs.totalMag;
            childs.minTurnout := minTurnout;
            childs.maxTurnout := maxTurnout;

            childs.showFraud := showFraud;
            childs.showProjected := showProjected;

            childs.fixWidth := fixWidth;

            childs.showJavascript := showJavascript;

            childs.votesUR_sum := fold 0 .. childs$-.votesUR_sum + childs.votesUR;
        }
    }
}

class CountryContainer(tweenMagnitude) : Node{
    evaluation {
        x := rx - w;
        y := by - h;
        
        render := (fixWidth != 0) ? x + y + w + h : 0;

        loop childs {
            childs.w := (childs.totalMag / totalMag) * w;
            childs.h := h;
            childs.rx := fold x .. childs$-.rx + childs.w;
            childs.by := y + h;
        }
    }
}

class Region(tweenMagnitude) : Node{
    evaluation {
        x := rx - w;
        y := by - h;

        render := 0;

        loop childs {

            childs.w := w;
            childs.h := (childs.totalMag / totalMag) * h;
            childs.rx := x + w;
            childs.by := fold y .. childs$-.by + childs.h;
        }
    }
}

class District(tweenMagnitude) : Node{
    evaluation {
        x := rx - w;
        y := by - h;
        
        render := (fixWidth != 0) ? x + y + w + h : 0;

        loop childs {
            childs.w := (childs.totalMag / totalMag) * w;
            childs.h := h;
            childs.rx := fold x .. childs$-.rx + childs.w;
            childs.by := y + h;
        }
    }
}

class VSquare(tweenMagnitude) : Node{
    evaluation {
        x := rx - w;
        y := by - h;

        render := 0;

        loop childs {
            childs.w := w;
            childs.h := (childs.totalMag / totalMag) * h;
            childs.rx := x + w;
            childs.by := fold y .. childs$-.by + childs.h;
        }
    }
}

class HSquare(tweenMagnitude) : Node{
    evaluation {
        x := rx - w;
        y := by - h;

        render := 0;

        loop childs {
            childs.w := (childs.totalMag / totalMag) * w;
            childs.h := h;
            childs.rx := fold x .. childs$-.rx + childs.w;
            childs.by := y + h;
        }
    }
}

class PollingPlace : Node{
    attributes {
        // Total number of ballots cast in this place
        input totalVotes : int;
        // Number of ballots cast for UR
        input totalVotesUR : int;
        // Percent of votes for UR
        input urVotes : int;
        // Average of district's percent of votes for UR (e.g., projected UR vote %)
        input urVotesProjected : int;
        // Percent of registered voters who cast a ballot in this place
        input turnout : int;

        // Default color (e.g., color when % of votes for UR is 0%)
        input defColor : int;
        // UR colors (e.g., color when % of votes for UR is 100%)
        input urColor : int; // Red
        // Color to turn fraudulent nodes
        input fraudColor : int;

        // Bool-like int to let us know if this node should be rendered in our JS simulation
        input inJavascript : int;

        output calcRegularColor : int;
        output calcFraudColor: int;
        output calcProjectedColor : int;
        output calcVotesColor : int;

        output magnitude : int;
    }

    evaluation {
        calcProjectedColor := defColor + urColor + urVotesProjected;
        calcRegularColor := defColor + urColor + urVotes;
        calcVotesColor := (turnout > 0) ? calcRegularColor + calcProjectedColor + showProjected : calcRegularColor;
        calcFraudColor := (turnout > 0) ? fraudColor : calcVotesColor;
        
        render := x + y + w + h + calcVotesColor + calcFraudColor + showFraud;

        x := rx - w;
        y := by - h;

        magnitude := (turnout > minTurnout && turnout <= maxTurnout) ? totalVotes : 0;
        totalMag := (inJavascript != 0) ? magnitude : showJavascript * magnitude;
        
        // How many votes for UR does this node contribute?
        // Turn to 0 if we're not showing this bin
        // If this is a suspect polling place, interpolate between real and
        // projected values based off showProjected.
        votesUR := (turnout > minTurnout && turnout <= maxTurnout) ? 
            ((turnout > 0) ? ((totalVotesUR * (1 - showProjected)) + ((totalVotes * urVotesProjected) * showProjected)) : totalVotesUR)
            : 0;
    }
    
}
