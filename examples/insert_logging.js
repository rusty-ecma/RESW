function Thing() {
    if (!new.target) {
        return new Thing();
    }
    this.add = function(one, two) {
        return one + two;
    }
}

function add(one, two) {
    return one + two;
}

class Stuff {
    constructor() {

    }

    add(one, two){
        class InnerStuff {
            constructor() {
    
            }
            add(three, four) {
                return three + four;
            }
        }
        return new InnerStuff().add(one, two);
    }
}

let x = (a) => a + 1;
let z = ({x, y}) => x + y;
(function() {
    let t = Thing();
    t.add(1, 2)
    let a = add(3, 4);
    let s = new Stuff();
    s.add(5, 6);
})();
