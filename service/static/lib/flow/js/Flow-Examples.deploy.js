smalltalk.addPackage('Flow-Examples', {});
smalltalk.addClass('ToDo', smalltalk.PersistentModel, ['name', 'description'], 'Flow-Examples');
smalltalk.addMethod(
unescape('_name'),
smalltalk.method({
selector: unescape('name'),
fn: function (){
var self=this;
return self['@name'];
return self;}
}),
smalltalk.ToDo);

smalltalk.addMethod(
unescape('_description'),
smalltalk.method({
selector: unescape('description'),
fn: function (){
var self=this;
return self['@description'];
return self;}
}),
smalltalk.ToDo);

smalltalk.addMethod(
unescape('_description_'),
smalltalk.method({
selector: unescape('description%3A'),
fn: function (aString){
var self=this;
(self['@description']=aString);
return self;}
}),
smalltalk.ToDo);

smalltalk.addMethod(
unescape('_name_'),
smalltalk.method({
selector: unescape('name%3A'),
fn: function (aString){
var self=this;
(self['@name']=aString);
return self;}
}),
smalltalk.ToDo);



smalltalk.addClass('ToDoList', smalltalk.PersistentModel, ['toDos'], 'Flow-Examples');
smalltalk.addMethod(
unescape('_toDos'),
smalltalk.method({
selector: unescape('toDos'),
fn: function (){
var self=this;
return (($receiver = self['@toDos']) == nil || $receiver == undefined) ? (function(){return smalltalk.send(self, "_initializeToDos", []);})() : $receiver;
return self;}
}),
smalltalk.ToDoList);

smalltalk.addMethod(
unescape('_initializeToDos'),
smalltalk.method({
selector: unescape('initializeToDos'),
fn: function (){
var self=this;
return (self['@toDos']=smalltalk.send((smalltalk.Array || Array), "_new", []));
return self;}
}),
smalltalk.ToDoList);

smalltalk.addMethod(
unescape('_addToDo_'),
smalltalk.method({
selector: unescape('addToDo%3A'),
fn: function (aToDo){
var self=this;
return smalltalk.send(smalltalk.send(self, "_toDos", []), "_add_", [aToDo]);
return self;}
}),
smalltalk.ToDoList);

smalltalk.addMethod(
unescape('_removeToDo_'),
smalltalk.method({
selector: unescape('removeToDo%3A'),
fn: function (aToDo){
var self=this;
smalltalk.send(smalltalk.send(self, "_toDos", []), "_remove_", [aToDo]);
return self;}
}),
smalltalk.ToDoList);


smalltalk.addMethod(
unescape('_example'),
smalltalk.method({
selector: unescape('example'),
fn: function (){
var self=this;
return (function($rec){smalltalk.send($rec, "_addToDo_", [(function($rec){smalltalk.send($rec, "_name_", ["Start with the problem"]);return smalltalk.send($rec, "_yourself", []);})(smalltalk.send((smalltalk.ToDo || ToDo), "_new", []))]);smalltalk.send($rec, "_addToDo_", [(function($rec){smalltalk.send($rec, "_name_", ["Show how this problem hurts"]);return smalltalk.send($rec, "_yourself", []);})(smalltalk.send((smalltalk.ToDo || ToDo), "_new", []))]);smalltalk.send($rec, "_addToDo_", [(function($rec){smalltalk.send($rec, "_name_", ["Present the solution"]);return smalltalk.send($rec, "_yourself", []);})(smalltalk.send((smalltalk.ToDo || ToDo), "_new", []))]);smalltalk.send($rec, "_addToDo_", [(function($rec){smalltalk.send($rec, "_name_", ["Exaplain the benefits of using it"]);return smalltalk.send($rec, "_yourself", []);})(smalltalk.send((smalltalk.ToDo || ToDo), "_new", []))]);return smalltalk.send($rec, "_yourself", []);})(smalltalk.send(self, "_new", []));
return self;}
}),
smalltalk.ToDoList.klass);

smalltalk.addMethod(
unescape('_oneOfToDosFrom_'),
smalltalk.method({
selector: unescape('oneOfToDosFrom%3A'),
fn: function (aReifiedJSON){
var self=this;
smalltalk.send((smalltalk.ToDo || ToDo), "_reifyFrom_", [aReifiedJSON]);
return self;}
}),
smalltalk.ToDoList.klass);


