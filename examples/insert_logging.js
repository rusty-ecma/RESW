function Thing() {
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

    add(one, two) {
        return one + two;
    }
}

let x = (a) => a + 1;

(function() {
    let t = Thing();
    let a = add(1, 2);
    let s = new Stuff();
    s.add(3, 4);
})();