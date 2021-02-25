'var friends = API.friends.get({"fields" : "domain, can_see_audio"});' +
'var count = %d;' +
'var offset = %d;' +
'var c = 0;' +
'var i = 0;' +
'var users = [];' +
'while (i < friends.count) {' +
'	if ((i >= offset) && (c < count))' +
'	{' +
'		if (friends.items[i].can_see_audio) {' +
'			var user = friends.items[i];' +
'			user.audio_count = API.audio.getCount(user.id);' +
'			users.push(friends.items[i]);' +
'			c = c + 1;' +
'		}' +
'	}' +
'	i = i + 1;' +
'}' +
'return {"count": friends.count, "items": users};'
