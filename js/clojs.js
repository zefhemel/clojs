/* 
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

function Cons(head, tail) {
    this.head = head;
    this.tail = tail;

    this.toArray = function() {
        var result = Array();
        result.push(this.head);
        var current = this.tail;
        while(current != null) {
            result.push(current.head);
            current = current.tail;
        }
        return result;
    }

    this.toString = function() {
        return this.toArray().toString();
    }
}