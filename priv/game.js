class SnakeState {}

SnakeState.ACTIVE = 'A';
SnakeState.COLLISION = 'C';

class GameState {}

GameState.ACTIVE = 'A';
GameState.PENDING = 'P';
GameState.INACTIVE = 'I';

class Snake {
	constructor(id, name, state, segments, color) {
		this.id = id;
		this.name = name;
		this.state = state;
		this.segments = segments;
		this.color = color;
	}
}

class Treasure {
	constructor(x, y, type) {
		this.x = x;
		this.y = y;
		this.type = type;
	}
}

class PendingGame {
	constructor(id, name, currentPlayers, maxPlayers) {
		this.id = id;
		this.name = name;
		this.currentPlayers = currentPlayers;
		this.maxPlayers = maxPlayers;
	}
}

class Game {
	constructor(context, gameList, snakeList) {
		this.snakes = {};
		this.treasures = [];
		this.context = context;
		this.sizeMultiplier = 10;
		this.state = GameState.INACTIVE;
		this.gameList = gameList;
		this.snakeList = snakeList;
		this.snakeColors = {};
		this.availableColors = this.getColors();
	}
	
	getColors () {
		return [
			'#213F55',
			'#246929',
			'#85682D',
			'#853C2D',
			'#742843'
		];
	}
	
	getSnakeColor (snakeId) {
		if (!(snakeId in this.snakeColors)) {						
			this.snakeColors[snakeId] = this.availableColors[Math.floor((Math.random(this.availableColors.length) * this.availableColors.length))];
			
			this.availableColors = this.availableColors.filter(color => color != this.snakeColors[snakeId]);
		}
		
		return this.snakeColors[snakeId];
	}
	
	addOrUpdateSnake(snake) {
		if (snake.id in this.snakes) {
			this.snakes[snake.id].segments = snake.segments;
			this.snakes[snake.id].state = snake.state;
		} else {
			this.snakes[snake.id] = snake;
			this.snakes[snake.id].color = this.getSnakeColor(snake.id);
		}
		
		this.haveWeLost();
	}
	
	haveWeLost() {
		if (this.playerId in this.snakes && this.snakes[this.playerId].state == SnakeState.COLLISION) {
			alert('YOU LOST!');
		}
	}
	
	handleMessage(message) {
		
		switch (message.constructor.name) {
			case 'UpdateMessage':
				var snakeIds = [];
			
				if (this.playerId == undefined) {
					return;
				}
			
				for (var snake of message.snakes) {
					this.addOrUpdateSnake(snake);
					
					snakeIds.push(snake.id);
				}
				
				//remove snakes which haven't been returned
				for (var snakeId in this.snakes) {
					if (snakeIds.indexOf(snakeId) < 0) {
						delete this.snakes[snakeId];
					}
				}
				
				this.treasures = message.treasures;

				this.snakeList.update(this, message.snakes);

				this.render();
			break;
			
			case 'IdMessage':
				this.playerId = message.id;
			break;
			
			case 'ListResponseMessage':
				console.log(message);
				this.gameList.update(message.games);
			break;
		}
	}
	
	coordsToSegment(x, y) {		
		return [
			x * this.sizeMultiplier,
			y * this.sizeMultiplier,
			this.sizeMultiplier,
			this.sizeMultiplier
		];
	}
	
	render() {

		this.context.clearRect(0, 0, this.context.canvas.clientWidth, this.context.canvas.clientHeight);
		
		for (var snakeId in this.snakes) {
			var snake = this.snakes[snakeId];
			
			this.context.fillStyle = snake.color;
			
			for (var segment of snake.segments) {				
				this.context.fillRect.apply(this.context, this.coordsToSegment(segment[0], segment[1]));
			}
		}
		
		this.context.fillStyle = '#DE0A82';
		
		for (var treasure of this.treasures) {
			this.context.fillRect.apply(this.context, this.coordsToSegment(treasure.x, treasure.y));
		}
	}
}

class UpdateMessage {
	constructor(snakes, treasures) {
		this.snakes = snakes;
		this.treasures = treasures;
	}
}

class IdMessage {
	constructor(id) {
		this.id = id;
	}
}

class ListMessage { }

class LeaveMessage { }

class ListResponseMessage {
	constructor(games) {
		this.games = games;
	}
}

class StartMessage {
	constructor(width, height, gameName, maxPlayers) {
		this.width = width;
		this.height = height;
		this.gameName = gameName;
		this.maxPlayers = maxPlayers;
	}
}

class JoinMessage {
	constructor(gameId, playerName) {
		this.gameId = gameId;
		this.playerName = playerName;
	}
}

class DirectionMessage {
	constructor(direction) {
		this.direction = direction;
	}
}

class MessageDeserializer {
	static deserialize(payload) {
		switch (payload.slice(0, 2)) {
			case 'D#':
				var updateTexts = payload.slice(2).split('_'),
					treasureTexts = updateTexts[1].split('|'),
					snakeTexts = updateTexts[2].split('|'),
					snakes = [],
					treasures = [];

				for (var snakeText of snakeTexts) {
					var snakeParts = snakeText.split('='),
						snakeId = snakeParts[0],
						snakeName = snakeParts[1],
						snakeState = snakeParts[2],
						segmentTexts = snakeParts[3].split(';'),
						segments = [],
						snake;
					
					segments = segmentTexts.map(function (segmentText) {
						var XY = segmentText.split(',');
						
						return [parseFloat(XY[0]), parseFloat(XY[1])];
					});
					
					snake = new Snake(snakeId, snakeName, snakeState, segments);
					
					snakes.push(snake);
				}
				
				for (var treasureText of treasureTexts) {		
					if (treasureText.length == 0) {
						continue;
					}
								
					var treasureParts = treasureText.split('='),
						treasureType = treasureParts[0],
						XY = treasureParts[1].split(','),
						treasure;
				
					treasure = new Treasure(parseFloat(XY[0]), parseFloat(XY[1]), treasureType);
					
					treasures.push(treasure);
				}
				
				return new UpdateMessage(snakes, treasures);
			break;
			
			case 'L#':
				if (payload.slice(2).length == 0) {
					return new ListResponseMessage([]);
				} else {
					var gameTexts = payload.slice(2).split(';'),
						games = [];
	
					for (var gameText of gameTexts) {
						games.push(new PendingGame(gameText.split(',')[0], gameText.split(',')[1], gameText.split(',')[2], gameText.split(',')[3]));
					}
					
					return new ListResponseMessage(games);
				}

			break;
			
			//the server tells us what our ID is
			case 'I#':
				return new IdMessage(payload.slice(2));
			break;
			
			default:
			
			break;
			
		}
	}
	
	static serialize(message) {
		switch (message.constructor.name) {
			case 'StartMessage':
				return 'S#' + message.width + ',' + message.height + ',' + message.gameName + ',' + message.maxPlayers;
			break;
			
			case 'JoinMessage':
				return 'J#' + message.gameId + ',' + message.playerName;
			break;
			
			case 'DirectionMessage':
				return 'D#' + message.direction;
			break;
			
			case 'ListMessage':
				return 'L#';
			break;
			
			case 'LeaveMessage':
				return 'Q#';
			break;
		}
	}
}

class Communication {
	constructor(ws, game) {
		this.ws = ws;
		this.game = game;
	}
	
	init() {
		this.ws.onmessage = (e) => {
			var payload = e.data;
			console.log("Received: " + payload);

			var message = MessageDeserializer.deserialize(payload);
			
			if (typeof message != 'object') {
				return;
			}

			this.game.handleMessage(message);
		};
		
		this.ws.onclose = (e) => {
			console.log('Connection closed');
		};
	}
	
	send(message) {
		var payload = MessageDeserializer.serialize(message);
		
		console.log("Sending: " + payload);
		
		return this.ws.send(payload);
	} 
}

class GameList {
	constructor(element) {
		this.element = element;
	}
	
	update(games) {
		this.element.innerHTML = '';
		
		for (var game of games) {
			var a = document.createElement('a'),
				li = document.createElement('li');
			
			if (game.currentPlayers < game.maxPlayers) {
				a.setAttribute('href', '#' + game.id);
				a.classList.add('join-link');
				a.textContent = game.name + ' [' + game.currentPlayers + '/' + game.maxPlayers  + ']';
				
				li.appendChild(a);
			} else {
				li.textContent = game.name + ' [' + game.currentPlayers + '/' + game.maxPlayers  + ']';
			}

			this.element.appendChild(li);
		}
	}
}

class SnakeList {
	constructor(element) {
		this.element = element;
	}
	
	update(game, snakes) {
		this.element.innerHTML = '';
		
		for (var snake of snakes) {
			var li = document.createElement('li');
			
			li.textContent = snake.name + ' [' + (snake.segments.length  < 2 ? 0 : snake.segments.length - 2)  + ']';
			li.style.color = game.getSnakeColor(snake.id);

			this.element.appendChild(li);
		}
	}
}

window.addEventListener('load', function () {
	var canvas = document.getElementById('board');
	
	if (!('WebSocket' in window)) {
		alert('This browser does not support WebSockets');
		return;
	}
	
	/* @todo: Change to your own server IP address */
	ws = new WebSocket("ws://"+window.location.host+"/websocket");

	ws.onopen = function() {
		console.log('Connected');
	};
		
	var game = new Game(
		canvas.getContext('2d'),
		new GameList(document.getElementById('games-list')),
		new SnakeList(document.getElementById('snakes-list'))
	);
	var communication = new Communication(ws, game);
	
	communication.init();
	
	window.addEventListener('keydown', function (e) {
		
		switch (e.keyCode) {
			case 38:
				e.preventDefault();
				communication.send(new DirectionMessage(1));
			break;
			
			case 40:
				e.preventDefault();
				communication.send(new DirectionMessage(3));
			break;
			
			case 37:
				e.preventDefault();
				communication.send(new DirectionMessage(4));
			break;
			
			case 39:
				e.preventDefault();
				communication.send(new DirectionMessage(2));
			break;
			
		}
	});
	
	document.getElementById('username-form').addEventListener('submit', function (e) {
		e.preventDefault();
		
		game.username = e.target.elements[0].value;
		
		switchRightPane('settings-pane');
		
		communication.send(new ListMessage());
	});
	
	function switchLeftPane(pane) {
		var panes = ['game-pane', 'placeholder-pane'];
		
		panes.map((currentPane) => document.getElementById(currentPane).style.display = 'none');
		document.getElementById(pane).style.display = 'block';
	}
	
	function switchRightPane(pane) {
		var panes = ['settings-pane', 'status-pane', 'username-pane'];
		
		panes.map((currentPane) => document.getElementById(currentPane).style.display = 'none');
		document.getElementById(pane).style.display = 'block';
	}
	
	document.getElementById('create-form').addEventListener('submit', function (e) {
		e.preventDefault();
		
		if (game.state == GameState.INACTIVE) {
			game.state = GameState.PENDING;
			
			communication.send(new StartMessage(60, 60, game.username, e.target.elements[0].value));
			
			switchRightPane('status-pane');
			
			switchLeftPane('game-pane');
		}
	});
	
	document.getElementById('games-list').addEventListener('click', function (e) {
		if (e.target && e.target.classList.contains('join-link')) {
			game.state = GameState.PENDING;
			
			communication.send(new JoinMessage(e.target.getAttribute('href').substr(1), game.username));
			
			switchRightPane('status-pane');
			
			switchLeftPane('game-pane');
		} else {
			console.log(e);
		}
	});
	
	document.getElementById('leave-link').addEventListener('click', function (e) {
		if (game.state != GameState.INACTIVE) {
			communication.send(new LeaveMessage());
			
			switchRightPane('settings-pane');
			
			switchLeftPane('placeholder-pane');
		}
	});
});
