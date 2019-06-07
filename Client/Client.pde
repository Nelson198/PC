import java.io.InputStreamReader;
import java.net.Socket;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.Arrays;
import processing.sound.*;

// Variáveis que guardam os ícones e os sons da interface
ArrayList<PShape> icons = new ArrayList<PShape>();
ArrayList<SoundFile> sounds = new ArrayList<SoundFile>();

// Variáveis para serem usadas para comunicar com o servidor através de um socket
Socket s;
BufferedReader fromSocket;
PrintWriter toSocket;
Thread reader = null;

// Objeto que guarda toda a informação da partida atual
Estado estado = new Estado();

AtomicBoolean mainMenu = new AtomicBoolean(true), loginMenu = new AtomicBoolean(false), registerMenu = new AtomicBoolean(false), unregisterMenu = new AtomicBoolean(false), waitMatch = new AtomicBoolean(false), leaderboardMenu = new AtomicBoolean(false), matchMenu = new AtomicBoolean(false), bestScoresMenu = new AtomicBoolean(false), messageMenu = new AtomicBoolean(false), selectionMenu = new AtomicBoolean(false), onlineMenu = new AtomicBoolean(false), optionsMenu = new AtomicBoolean(false);
AtomicBoolean usr = new AtomicBoolean(true), pwd = new AtomicBoolean(false), success = new AtomicBoolean(false), showScores = new AtomicBoolean(false);
AtomicBoolean musicOn = new AtomicBoolean(false), iconsOn = new AtomicBoolean(false);

String message = "";
int x = 400;
int y = 50;
String usrText = "";
String pwdText = "";

boolean inRect(float x, float y, float w, float h) {
  if ((x <= mouseX && mouseX <= x + w) && (y <= mouseY && mouseY <= y + h))
    return true;
  else
    return false;
}

String coded(String text) {
  String res = "";
  for(int i=0; i<text.length(); i++)
    res += "*";
  return res;
}

String getChar(String myText) {
  if (keyCode == BACKSPACE) {
    if (myText.length() > 0)
      myText = myText.substring(0, myText.length()-1);
  } else if (keyCode == DELETE)
    myText = "";
  else if (keyCode != SHIFT && keyCode != CONTROL && keyCode != ALT)
    myText = myText + key;
  return myText;
}

void mouseClicked() {
  if(messageMenu.get()) {
    if (inRect(width/2 - x/2, height/2 + 3*y, x, y)) {
      if(!success.get())
        mainMenu.set(true);
      message = "";
      success.set(false);
      messageMenu.set(false);
    }
  } else if(mainMenu.get()) {
    // Login
    if (inRect(width/2 - x/2, height/2 - y, x, y)) {
        loginMenu.set(true);
        mainMenu.set(false);
    // Register
    } else if (inRect(width/2 - x/2, height/2 + y, x, y)) {
        registerMenu.set(true);
        mainMenu.set(false);
    // Unregister
    } else if (inRect(width/2 - x/2, height/2 + 3*y, x, y)) {
        unregisterMenu.set(true);
        mainMenu.set(false);
    // Scoreboard
    } else if (inRect(width/2 - x/2, height/2 + 5*y, x, y)) {
        try {
          toSocket.println("Scores");
          toSocket.flush();
          fromSocket.readLine(); // Ignorar "BeginScores"
          String info = fromSocket.readLine();
          Estado e = estado.get();
          e.bestScores = new ArrayList<Score>();
          while (!info.equals("EndScores")) {
              String[] fields = info.split(":");
              e.bestScores.add(new Score(fields[1], Float.parseFloat(fields[2])));
              info = fromSocket.readLine();
          }
          estado.set(e);
        } catch(Exception ex) {}
        bestScoresMenu.set(true);
        mainMenu.set(false);
    } else if (inRect(0.01*width, 0.9*height, x/2, y)) {
      optionsMenu.set(true);
      mainMenu.set(false);
    }
  } else if (optionsMenu.get()) {
    if (inRect(width/2 - x/2, height/2 - y, x, y))
      if (iconsOn.get()) {
        iconsOn.set(false);
        icons = new ArrayList<PShape>();
      } else
        turnOnIcons();
    else if (inRect(width/2 - x/2, height/2 + y, x, y))
      if (musicOn.get()) {
        musicOn.set(false);
        if (sounds.size() > 0) {
          for(SoundFile s: sounds)
            s.stop();
          sounds = new ArrayList<SoundFile>();
        }
      } else
        turnOnMusic();
    else if (inRect(width/2 - x/2, height/2 + 6*y, x, y)) {
      optionsMenu.set(false);
      mainMenu.set(true);
    }
  } else if(loginMenu.get() || registerMenu.get() || unregisterMenu.get()) {
    // Back
    if (inRect(width/2 - x/2, height/2 + 3*y, x, y)) {
        mainMenu.set(true);
        usrText = "";
        pwdText = "";
        usr.set(true);
        pwd.set(false);
        if(loginMenu.get())
          loginMenu.set(false);
        else if(registerMenu.get())
          registerMenu.set(false);
        else if(unregisterMenu.get())
          unregisterMenu.set(false);
    }
  } else if (onlineMenu.get()) {
    // Play
    if (inRect(width/2 - x/2, height/2 - y, x, y)) {
        onlineMenu.set(false);
        waitMatch.set(true);
        switchMusic(1);
        try {
          if (reader == null) {
            // Criar uma thread para ler do socket
            reader = new Reader(fromSocket, usrText, estado.player.getIconIdx());
            reader.start();
          }
        } catch (Exception e) {
          exit();
        }
    // Select Avatar
    } else if (inRect(width/2 - x/2, height/2 + y, x, y) && iconsOn.get()) {
        onlineMenu.set(false);
        selectionMenu.set(true);
    }
  } else if (selectionMenu.get()) {
    float xOff = width/8;
    float yOff = height/2.5;

    for(int i = 0; i < icons.size(); i++) {
      if (inRect(xOff-50, yOff-50, 100, 100)) {
        Estado e = estado.get();
        e.player.setIconIdx(i);
        estado.set(e);
        onlineMenu.set(true);
        selectionMenu.set(false);
        break;
      }
      xOff += width/4;
      if (xOff > 7*width/8) {
        xOff = width/8;
        yOff += height/3;
      }
    }

    if (inRect(width/2 - x/2, height/2 + 6*y, x, y)) {
      onlineMenu.set(true);
      selectionMenu.set(false);
    }

  } else if(bestScoresMenu.get()) {
    if (inRect(width/2 - x/2, height/2 + 5*y, x, y)) {
      mainMenu.set(true);
      bestScoresMenu.set(false);
    }
  } else if(leaderboardMenu.get()) {
    if (inRect(width/2 - x - x/5, height/2 + 3*y, x, y)) {
      mainMenu.set(true);
      leaderboardMenu.set(false);
      switchMusic(0);
      toSocket.println("Quit");
      toSocket.flush();
      usrText = "";
      reader = null;
    } else if (inRect(width/2 + x/5, height/2 + 3*y, x, y)) {
      onlineMenu.set(true);
      leaderboardMenu.set(false);
      switchMusic(0);
      toSocket.println("Continue");
      toSocket.flush();
      reader = null;
    }
  }
}

void keyPressed() {
  if(matchMenu.get()) {
    if (key == 's')
      showScores.set(!showScores.get());
    if (key == CODED) {
      if (keyCode == UP)
        toSocket.println("keyChanged:up:true");

      if (keyCode == DOWN)
        toSocket.println("keyChanged:down:true");

      if (keyCode == LEFT)
        toSocket.println("keyChanged:left:true");

      if (keyCode == RIGHT)
        toSocket.println("keyChanged:right:true");

      toSocket.flush();
    }
  } else if(loginMenu.get() || registerMenu.get() || unregisterMenu.get()) {  
      if(usr.get()){
        if(keyCode == ENTER) {
          usr.set(false);
          pwd.set(true);
        } else
          usrText = getChar(usrText);
      } else if(pwd.get()) {
        if(keyCode == ENTER) {
          pwd.set(false);
          if(loginMenu.get()) {
              login(usrText, pwdText);
          } else if(registerMenu.get()) {
              register(usrText, pwdText);
          } else if(unregisterMenu.get()) {            
              unregister(usrText, pwdText);
          }
        } else
            pwdText = getChar(pwdText);
      }
  }
}

void keyReleased() {
  if (matchMenu.get()) {
    if (key == CODED) {
      if (keyCode == UP)
        toSocket.println("keyChanged:up:false");

      if (keyCode == DOWN)
        toSocket.println("keyChanged:down:false");

      if (keyCode == LEFT)
        toSocket.println("keyChanged:left:false");

      if (keyCode == RIGHT)
        toSocket.println("keyChanged:right:false");

      toSocket.flush();
    }
  }
}

void switchMusic(int idx) {
  if(!musicOn.get())
    return;
  for(int i = 0; i < sounds.size(); i++)
    if(i != idx && sounds.get(i).isPlaying())
      sounds.get(i).stop();
  if(idx == 0)
    sounds.get(idx).loop();
  else
    sounds.get(idx).play();
}

void turnOnMusic(){
  musicOn.set(true);
  SoundFile mainMenuSound = new SoundFile(this, "Music/MainMenu.mp3");
  SoundFile waitMenuSound = new SoundFile(this, "Music/WaitMenu.mp3");
  waitMenuSound.amp(0.2);
  SoundFile matchMenuSound = new SoundFile(this, "Music/MatchMenu.mp3");
  matchMenuSound.amp(0.2);
  SoundFile winnerSound = new SoundFile(this, "Music/Winner.mp3");
  SoundFile looserSound = new SoundFile(this, "Music/Looser.mp3");
  looserSound.amp(0.3);

  SoundFile[] soundsArray = {mainMenuSound, waitMenuSound, matchMenuSound, winnerSound, looserSound};
  sounds = new ArrayList<SoundFile>(Arrays.asList(soundsArray));
  mainMenuSound.loop();
}

void turnOnIcons() {
  iconsOn.set(true);
  shapeMode(CENTER);
  for(int i = 1; i < 8; i++){
    PShape sh = loadShape("e"+i+".svg");
    icons.add(sh);
  }
}

void setup() {
  try {
    // Conectar com o servidor e criar um socket
    s = new Socket("localhost", 1234);

    // Criar um objeto para ler do socket e um para escrever para o socket
    fromSocket = new BufferedReader(new InputStreamReader(s.getInputStream()));
    toSocket = new PrintWriter(s.getOutputStream());
  } catch (Exception e) {
    showMessageDialog(null, "Não foi possível conectar com o servidor!", "Erro", INFORMATION_MESSAGE);
    exit();
    return;
  }

  //turnOnMusic();
  //turnOnIcons();
  fullScreen(P2D);
}

void draw() {
  background(211,211,211);
  if (messageMenu.get())
    showMessage();
  else if (mainMenu.get())
    showMainMenu();
  else if (loginMenu.get() || registerMenu.get() || unregisterMenu.get())
    showLoginRegisterUnregisterScreen();
  else if (optionsMenu.get())
    showOptionsMenu();
  else if (onlineMenu.get())
    showOnlineMenu();
  else if (selectionMenu.get())
    showSelectionMenu();
  else if (waitMatch.get())
    showWaitMatchScreen();
  else if (bestScoresMenu.get())
    showBestScoresScreen();
  else if (matchMenu.get())
    showMatchScreen();
  else if (leaderboardMenu.get())
    showLeaderboardScreen();
}

void showOnlineMenu() {
  strokeWeight(0);
  stroke(211, 211, 211);
  if (inRect(width/2 - x/2, height/2 - y, x, y)) {
    strokeWeight(3.5);
    stroke(255, 255, 0);
  }
  fill(255, 60, 60);
  rect(width/2 - x/2, height/2 - y, x, y, 10);
  textSize(24);
  fill(0);
  textAlign(CENTER, CENTER);
  text("Play", width/2, height/2 - y/2);

  if (iconsOn.get()) {
    strokeWeight(0);
    stroke(211, 211, 211);
    if (inRect(width/2 - x/2, height/2 + y, x, y)) {
      strokeWeight(3.5);
      stroke(255, 255, 0);
    }
    fill(60, 255, 60);
    rect(width/2 - x/2, height/2 + y, x, y, 10);
    fill(0);
    textAlign(CENTER, CENTER);
    text("Select Avatar", width/2, height/2 + y + y/2);
  }
}

void showSelectionMenu() {
  textSize(50);
  fill(0);
  textAlign(CENTER,CENTER);
  text("Select Avatar", width/2, height/7);

  float xOff = width/8;
  float yOff = height/2.5;

  for(int i = 0; i < icons.size(); i++) {
    PShape avatar = icons.get(i);
    shape(avatar, xOff, yOff, 100, 100);
    xOff += width/4;
    if (xOff > 7*width/8) {
      xOff = width/8;
      yOff += height/3;
    }
  }

  strokeWeight(0);
  stroke(211, 211, 211);
  if (inRect(width/2 - x/2, height/2 + 6*y, x, y)) {
    strokeWeight(3.5);
    stroke(255, 255, 0);
  }
  fill(255);
  rect(width/2 - x/2, height/2 + 6*y, x, y, 10);
  fill(0);
  textSize(24);
  textAlign(CENTER, CENTER);
  text("Back", width/2, height/2 + 6*y + y/2);
}

void showMessage() {
  background(211, 211, 211);
  textSize(32);
  fill(0);
  textAlign(CENTER,CENTER);
  text(message, width/2, height/2);

  strokeWeight(0);
  stroke(211, 211, 211);
  if (inRect(width/2 - x/2, height/2 + 3*y, x, y)) {
    strokeWeight(3.5);
    stroke(255, 255, 0);
  }
  fill(255);
  rect(width/2 - x/2, height/2 + 3*y, x, y, 10);
  fill(0);
  textSize(24);
  textAlign(CENTER, CENTER);
  text("Ok", width/2, height/2 + 3*y + y/2 - 4);
}

void showOptionsMenu() {
  String iconsText, musicText;
  if (iconsOn.get())
    iconsText = "ON";
  else
    iconsText = "OFF";
  if (musicOn.get())
    musicText = "ON";
  else
    musicText = "OFF";
  
  textSize(50);
  fill(0);
  textAlign(CENTER,CENTER);
  text("Options", width/2, height/7);

  strokeWeight(0);
  stroke(211, 211, 211);
  if (inRect(width/2 - x/2, height/2 - y, x, y)) {
    strokeWeight(3.5);
    stroke(255, 255, 0);
  }
  if (iconsOn.get())
    fill(60, 255, 60);
  else
    fill(255, 60, 60);
  rect(width/2 - x/2, height/2 - y, x, y, 10);
  textSize(24);
  fill(0);
  textAlign(CENTER, CENTER);
  text("Icons: " + iconsText, width/2, height/2 - y/2);

  strokeWeight(0);
  stroke(211, 211, 211);
  if (inRect(width/2 - x/2, height/2 + y, x, y)) {
    strokeWeight(3.5);
    stroke(255, 255, 0);
  }
  if (musicOn.get())
    fill(60, 255, 60);
  else
    fill(255, 60, 60);
  rect(width/2 - x/2, height/2 + y, x, y, 10);
  fill(0);
  textAlign(CENTER, CENTER);
  text("Music: " + musicText, width/2, height/2 + y + y/2);


  strokeWeight(0);
  stroke(211, 211, 211);
  if (inRect(width/2 - x/2, height/2 + 6*y, x, y)) {
    strokeWeight(3.5);
    stroke(255, 255, 0);
  }
  fill(255);
  rect(width/2 - x/2, height/2 + 6*y, x, y, 10);
  fill(0);
  textAlign(CENTER, CENTER);
  text("Back", width/2, height/2 + 6*y + y/2);
}

void showMainMenu() {
  textSize(75);
  fill(0);
  textAlign(CENTER,CENTER);
  text("Buracos", width/2, height/5);

  strokeWeight(0);
  stroke(211, 211, 211);
  if (inRect(width/2 - x/2, height/2 - y, x, y)) {
    strokeWeight(3.5);
    stroke(255, 255, 0);
  }
  fill(255, 60, 60);
  rect(width/2 - x/2, height/2 - y, x, y, 10);
  textSize(24);
  fill(0);
  textAlign(CENTER, CENTER);
  text("Login", width/2, height/2 - y/2);

  strokeWeight(0);
  stroke(211, 211, 211);
  if (inRect(width/2 - x/2, height/2 + y, x, y)) {
    strokeWeight(3.5);
    stroke(255, 255, 0);
  }
  fill(60, 255, 60);
  rect(width/2 - x/2, height/2 + y, x, y, 10);
  fill(0);
  textAlign(CENTER, CENTER);
  text("Register", width/2, height/2 + y + y/2);

  strokeWeight(0);
  stroke(211, 211, 211);
  if (inRect(width/2 - x/2, height/2 + 3*y, x, y)) {
    strokeWeight(3.5);
    stroke(255, 255, 0);
  }
  fill(249, 131, 34);
  rect(width/2 - x/2, height/2 + 3*y, x, y, 10);
  fill(0);
  textAlign(CENTER, CENTER);
  text("Unregister", width/2, height/2 + 3*y + y/2);

  strokeWeight(0);
  stroke(211, 211, 211);
  if (inRect(width/2 - x/2, height/2 + 5*y, x, y)) {
    strokeWeight(3.5);
    stroke(255, 255, 0);
  }
  fill(60, 60, 255);
  rect(width/2 - x/2, height/2 + 5*y, x, y, 10);
  fill(0);
  textAlign(CENTER, CENTER);
  text("Scoreboard", width/2, height/2 + 5*y + y/2);

  strokeWeight(0);
  stroke(211, 211, 211);
  if (inRect(0.01*width, 0.9*height, x/2, y)) {
    strokeWeight(3.5);
    stroke(255, 255, 0);
  }
  fill(130);
  rect(0.01*width, 0.9*height, x/2, y, 10);
  fill(0);
  textAlign(CENTER, CENTER);
  text("Options", 0.01*width + x/4, 0.9*height + y/2);

  fill(0);
  textSize(10);
  text("Made by: JRI, RV, NT", width - 60, height - 10);
}

void showLoginRegisterUnregisterScreen() {
  textSize(50);
  textAlign(CENTER,CENTER);
  if(loginMenu.get()) {
    fill(255, 60, 60);
    text("Login", width/2, height/5);
  } else if(registerMenu.get()) {
    fill(60, 255, 60);
    text("Register", width/2, height/5);
  } else if(unregisterMenu.get()) {
    fill(249, 131, 34);
    text("Unregister", width/2, height/5);
  }
  
  strokeWeight(0);
  stroke(211, 211, 211);
  fill(255);
  rect(width/2 - x/2, height/2 - y, x, y);
  textSize(24);
  fill(0);
  textAlign(CENTER, CENTER);
  text(usrText, width/2, height/2 - y/2 - 4);
  
  strokeWeight(0);
  stroke(211, 211, 211);
  fill(255);
  rect(width/2 - x/2, height/2 + y, x, y);
  fill(0);
  textAlign(CENTER, CENTER);
  text(coded(pwdText), width/2, height/2 + y + y/2 - 4);
  
  strokeWeight(0);
  stroke(211, 211, 211);
  if (inRect(width/2 - x/2, height/2 + 3*y, x, y)) {
    strokeWeight(3.5);
    stroke(255, 255, 0);
  }
  fill(255);
  rect(width/2 - x/2, height/2 + 3*y, x, y, 10);
  fill(0);
  textAlign(CENTER, CENTER);
  text("Back", width/2, height/2 + 3*y + y/2 - 4);
}

void showWaitMatchScreen() {
  textSize(32);
  fill(255, 0, 0);
  textAlign(CENTER,CENTER);
  text("Waiting for opponent...", width/2, height/2);
}

void showBestScoresScreen() {
  Estado e = estado.get();
  ArrayList<Score> bestScores = e.bestScores;
  textSize(50);
  fill(60, 60, 255);
  textAlign(CENTER, CENTER);
  text("Best Scores", width/2, height/5);
  textSize(30);
  for (int i = 0; i < bestScores.size(); i++) {
    Score s = bestScores.get(i);
    fill(0);
    text((i+1) + "º: " + s.username + " - " + s.score, width/2, height/2 + i*50 - 60);
  }
  strokeWeight(0);
  stroke(211, 211, 211);
  if (inRect(width/2 - x/2, height/2 + 5*y, x, y)) {
    strokeWeight(3.5);
    stroke(255, 255, 0);
  }
  fill(255);
  rect(width/2 - x/2, height/2 + 5*y, x, y, 10);
  fill(0);
  textSize(24);
  textAlign(CENTER, CENTER);
  text("Back", width/2, height/2 + 5*y + y/2);
}

void showLeaderboardScreen() {
  Estado e = estado.get();
  ArrayList<Score> leaderboard = e.leaderboard;
  textSize(50);
  fill(0);
  textAlign(CENTER,CENTER);
  text("Leaderboard", width/2, height/5);
  textSize(40);
  for (int i = 0; i < leaderboard.size(); i++) {
    Score s = leaderboard.get(i);
    fill(0);
    if (s.username.equals(e.player.getUsername()))
      fill(0, 0, 255);
    text((i+1) + "º: " + s.username + " - " + s.score, width/2, height/2 + i*60 - 60);
  }

  strokeWeight(0);
  stroke(211, 211, 211);
  if (inRect(width/2 - x - x/5, height/2 + 3*y, x, y)) {
    strokeWeight(3.5);
    stroke(255, 255, 0);
  }
  fill(255, 60, 60);
  rect(width/2 - x - x/5, height/2 + 3*y, x, y, 10);
  fill(0);
  textSize(24);
  text("Exit", width/2 - x/2 - x/5, height/2 + 3*y + y/2);

  strokeWeight(0);
  stroke(211, 211, 211);
  if (inRect(width/2 + x/5, height/2 + 3*y, x, y)) {
    strokeWeight(3.5);
    stroke(255, 255, 0);
  }
  fill(60, 255, 60);
  rect(width/2 + x/5, height/2 + 3*y, x, y, 10);
  fill(0);
  textSize(24);
  text("Continue", width/2 + x/2 + x/5, height/2 + 3*y + y/2);
}

void showMatchScreen() {
  // Pintar o plano de fundo de branco
  background(255);

  // Extrair as informações do estado atual
  Estado e = estado.get();
  Player player = e.player;
  ArrayList<Player> enemies = e.enemies;
  ArrayList<Blob> blobs = e.blobs;
  ArrayList<Score> bestScores = e.bestScores;
  int beginTime = e.beginTime;
  int currentTime = e.currentTime;

  // Desenhar as entidades do jogo
  for (int i = 0; i < blobs.size(); i++)
    blobs.get(i).show();

  ArrayList<Player> players = new ArrayList<Player>(enemies);
  players.add(player);
  players.sort(null);
  for (int i = 0; i < players.size(); i++)
    players.get(i).show();

  if(showScores.get()){
    // Melhores pontuações
    for (int i = 0; i < bestScores.size(); i++) {
      Score s = bestScores.get(i);
      fill(0);
      if (s.username.equals(e.player.getUsername()))
        fill(0, 0, 255);
      textSize(24);
      textAlign(LEFT, TOP);
      text(s.username + ": " + s.score, 3, 3 + i*30);
    }
  }

  // Relógio do jogo
  int time = currentTime - beginTime;
  int minutes = floor(time/60);
  int seconds = time - minutes*60;
  String minS, secS;

  if (minutes > 9)
    minS = str(minutes);
  else
    minS = "0" + minutes;

  if (seconds > 9)
    secS = str(seconds);
  else
    secS = "0" + seconds;
  fill(0);
  textSize(24);
  textAlign(RIGHT, TOP);
  text(minS + ":" + secS, width - 3, 3);

}

void exit() {
  try {
    s.close();
  } catch (Exception e) {}
  super.exit();
}
