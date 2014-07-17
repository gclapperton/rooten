/**
 * @constructor
 * @param {(jQuerySelector|Element|Object|Array.<Element>|jQuery|string|
 *     function())=} arg1
 * @param {(Element|jQuery|Document|
 *     Object.<string, (string|function(!jQuery.event=))>)=} arg2
 * @return {!jQuery}
 */
function jQuery(arg1, arg2) {}

/**
 * @constructor
 * @extends {jQuery}
 * @param {(jQuerySelector|Element|Object|Array.<Element>|jQuery|string|
 *     function())=} arg1
 * @param {(Element|jQuery|Document|
 *     Object.<string, (string|function(!jQuery.event=))>)=} arg2
 * @return {!jQuery}
 */
function $(arg1, arg2) {}

/**
 * @param {(string|function(number,String))} arg1
 * @return {!jQuery}
 */
jQuery.prototype.addClass = function(arg1) {};

/**
 * @param {(string|function(number,string))=} arg1
 * @return {!jQuery}
 */
jQuery.prototype.removeClass = function(arg1) {};

/**
 * @param {(string|jQuery.event)} arg1
 * @param {...*} var_args
 * @return {!jQuery}
 */
jQuery.prototype.trigger = function(arg1, var_args) {};
