Nous avons décidé de travailler par catégorie (arithmétique, classes, etc.) et non pas par fichier. Cela nous semblait plus pratique et convenable pour la réalisation du projet.
Chacun a travaillé de son côté la plupart du temps, ce qui n'a pas empêché de nous concerter sur quelques extensions et sur certaines difficultés un peu plus gênantes telles que bugs, questions, etc.
Nous avons fini la partie basique du projet à savoir, l'arithmétique, les variables, les instructions, les classes et attributs, les méthodes et l'héritage, dans tous les fichiers, dont l'interprète, le lexer, le parser et le vérificateur de types.
Nous avons de plus rajouté les extensions suivantes:

- Les champs immuables (final)
- Les visibilités sur les attributs et les méthodes (public, protected, private)
- Déclaration avec valeur initiale (int x = 0; )
- Déclaration en série (int x, y = 0, z, ... ; )
- Champ statiques (en revanche, accès uniquement à travers les instances de classe et non les classes elles-mêmes. Mais les attributs static ont quand meme une mémoire partagée)
- Super (le constructeur super, mais aussi le mot clé qui donne accès à la classe parent)
- Égalité et inégalité structurelle

Les extensions ont été codées sur tous les fichiers elles sont donc complètes et utilisables. En ce qui concerne certaines extensions, nous avons aussi dû modifier d'autres fichiers tels que kawai.ml.
Vous pouvez consulter les fichiers qui ont été modifiés pour chacune des extensions, dans le log des commits.
