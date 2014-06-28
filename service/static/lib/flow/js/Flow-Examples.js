smalltalk.addPackage('Flow-Examples', {});
smalltalk.addClass('ToDo', smalltalk.PersistentModel, ['name', 'description'], 'Flow-Examples');
smalltalk.addMethod(
unescape('_name'),
smalltalk.method({
selector: unescape('name'),
category: 'accessing',
fn: function (){
var self=this;
return self['@name'];
return self;},
args: [],
source: unescape('name%0A%0A%09%5E%20name'),
messageSends: [],
referencedClasses: []
}),
smalltalk.ToDo);

smalltalk.addMethod(
unescape('_description'),
smalltalk.method({
selector: unescape('description'),
category: 'accessing',
fn: function (){
var self=this;
return self['@description'];
return self;},
args: [],
source: unescape('description%0A%0A%09%5E%20description'),
messageSends: [],
referencedClasses: []
}),
smalltalk.ToDo);

smalltalk.addMethod(
unescape('_description_'),
smalltalk.method({
selector: unescape('description%3A'),
category: 'accessing',
fn: function (aString){
var self=this;
(self['@description']=aString);
return self;},
args: ["aString"],
source: unescape('description%3A%20aString%0A%0A%09description%20%3A%3D%20aString'),
messageSends: [],
referencedClasses: []
}),
smalltalk.ToDo);

smalltalk.addMethod(
unescape('_name_'),
smalltalk.method({
selector: unescape('name%3A'),
category: 'accessing',
fn: function (aString){
var self=this;
(self['@name']=aString);
return self;},
args: ["aString"],
source: unescape('name%3A%20aString%0A%0A%09name%20%3A%3D%20aString'),
messageSends: [],
referencedClasses: []
}),
smalltalk.ToDo);



smalltalk.addClass('ToDoList', smalltalk.PersistentModel, ['toDos'], 'Flow-Examples');
smalltalk.addMethod(
unescape('_toDos'),
smalltalk.method({
selector: unescape('toDos'),
category: 'accessing',
fn: function (){
var self=this;
return (($receiver = self['@toDos']) == nil || $receiver == undefined) ? (function(){return smalltalk.send(self, "_initializeToDos", []);})() : $receiver;
return self;},
args: [],
source: unescape('toDos%0A%0A%09%5E%20toDos%20ifNil%3A%5Bself%20initializeToDos%5D'),
messageSends: ["ifNil:", "initializeToDos"],
referencedClasses: []
}),
smalltalk.ToDoList);

smalltalk.addMethod(
unescape('_initializeToDos'),
smalltalk.method({
selector: unescape('initializeToDos'),
category: 'initialization',
fn: function (){
var self=this;
return (self['@toDos']=smalltalk.send((smalltalk.Array || Array), "_new", []));
return self;},
args: [],
source: unescape('initializeToDos%0A%0A%09%5E%20toDos%20%3A%3D%20Array%20new'),
messageSends: ["new"],
referencedClasses: ["Array"]
}),
smalltalk.ToDoList);

smalltalk.addMethod(
unescape('_addToDo_'),
smalltalk.method({
selector: unescape('addToDo%3A'),
category: 'actions',
fn: function (aToDo){
var self=this;
return smalltalk.send(smalltalk.send(self, "_toDos", []), "_add_", [aToDo]);
return self;},
args: ["aToDo"],
source: unescape('addToDo%3A%20aToDo%0A%0A%09%5E%20self%20toDos%20add%3A%20aToDo'),
messageSends: ["add:", "toDos"],
referencedClasses: []
}),
smalltalk.ToDoList);

smalltalk.addMethod(
unescape('_removeToDo_'),
smalltalk.method({
selector: unescape('removeToDo%3A'),
category: 'actions',
fn: function (aToDo){
var self=this;
smalltalk.send(smalltalk.send(self, "_toDos", []), "_remove_", [aToDo]);
return self;},
args: ["aToDo"],
source: unescape('removeToDo%3A%20aToDo%0A%0A%09self%20toDos%20remove%3A%20aToDo'),
messageSends: ["remove:", "toDos"],
referencedClasses: []
}),
smalltalk.ToDoList);


smalltalk.addMethod(
unescape('_example'),
smalltalk.method({
selector: unescape('example'),
category: 'actions',
fn: function (){
var self=this;
return (function($rec){smalltalk.send($rec, "_addToDo_", [(function($rec){smalltalk.send($rec, "_name_", ["Start with the problem"]);return smalltalk.send($rec, "_yourself", []);})(smalltalk.send((smalltalk.ToDo || ToDo), "_new", []))]);smalltalk.send($rec, "_addToDo_", [(function($rec){smalltalk.send($rec, "_name_", ["Show how this problem hurts"]);return smalltalk.send($rec, "_yourself", []);})(smalltalk.send((smalltalk.ToDo || ToDo), "_new", []))]);smalltalk.send($rec, "_addToDo_", [(function($rec){smalltalk.send($rec, "_name_", ["Present the solution"]);return smalltalk.send($rec, "_yourself", []);})(smalltalk.send((smalltalk.ToDo || ToDo), "_new", []))]);smalltalk.send($rec, "_addToDo_", [(function($rec){smalltalk.send($rec, "_name_", ["Exaplain the benefits of using it"]);return smalltalk.send($rec, "_yourself", []);})(smalltalk.send((smalltalk.ToDo || ToDo), "_new", []))]);return smalltalk.send($rec, "_yourself", []);})(smalltalk.send(self, "_new", []));
return self;},
args: [],
source: unescape('example%0A%0A%09%5E%20self%20new%20%0A%09%09addToDo%3A%20%28ToDo%20new%20%0A%09%09%09%09%09name%3A%20%27Start%20with%20the%20problem%27%3B%0A%09%09%09%09%09yourself%29%3B%0A%09%09addToDo%3A%20%28ToDo%20new%20%0A%09%09%09%09%09name%3A%20%27Show%20how%20this%20problem%20hurts%27%3B%0A%09%09%09%09%09yourself%29%3B%0A%09%09addToDo%3A%20%28ToDo%20new%20%0A%09%09%09%09%09name%3A%20%27Present%20the%20solution%27%3B%0A%09%09%09%09%09yourself%29%3B%0A%09%09addToDo%3A%20%28ToDo%20new%20%0A%09%09%09%09%09name%3A%20%27Exaplain%20the%20benefits%20of%20using%20it%27%3B%0A%09%09%09%09%09yourself%29%3B%0A%09%09yourself'),
messageSends: ["addToDo:", "name:", "yourself", "new"],
referencedClasses: ["ToDo"]
}),
smalltalk.ToDoList.klass);

smalltalk.addMethod(
unescape('_oneOfToDosFrom_'),
smalltalk.method({
selector: unescape('oneOfToDosFrom%3A'),
category: 'actions',
fn: function (aReifiedJSON){
var self=this;
smalltalk.send((smalltalk.ToDo || ToDo), "_reifyFrom_", [aReifiedJSON]);
return self;},
args: ["aReifiedJSON"],
source: unescape('oneOfToDosFrom%3A%20aReifiedJSON%0A%0A%09ToDo%20reifyFrom%3A%20aReifiedJSON'),
messageSends: ["reifyFrom:"],
referencedClasses: ["ToDo"]
}),
smalltalk.ToDoList.klass);


