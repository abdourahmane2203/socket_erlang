# socket_erlang


Pour lancer le programme sur un terminal windows on tape les commandes suivantes :
on se positionne sur le dossier ou se situe le fichier test.erl,
on saisit : erlc test.erl ==> pour compiler le programme
erl test pour exécuter le programme, pour lancer le serveur socket on tape la commande suivante : test:run(4000).

Ceci permet de lancer le socket. Et plusieurs clients peuvent se connecter en même temps.

Un client peut se connecter et saisir un message.
J'ai écris le code pour la diffusion des messages à tous les clients. Mais ceci ne fonctionne pas correctement.

Voici le bout de code pour la diffusion de messages :  
    {broadcast,From,Msg} ->
      BroadList = lists:filter(fun(Client) ->
        Client =/= From

