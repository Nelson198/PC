class Estado {
  Player player;
  ArrayList<Player> enemies;
  ArrayList<Blob> blobs;
  ArrayList<Score> bestScores;
  ArrayList<Score> leaderboard;
  int beginTime, currentTime;

  Estado() {
    this.player = new Player();
    this.enemies = new ArrayList<Player>();
    this.blobs = new ArrayList<Blob>();
    this.bestScores = new ArrayList<Score>();
    this.leaderboard = new ArrayList<Score>();
    this.beginTime = 0;
    this.currentTime = 0;
  }

  synchronized Estado get() {
    Estado res = new Estado();
    res.player = this.player.clone();
    
    ArrayList<Player> players = new ArrayList<Player>();
    for(Player p: this.enemies) {
      players.add(p.clone());
    }
    res.enemies = players;

    ArrayList<Blob> blobs = new ArrayList<Blob>();
    for(Blob b: this.blobs) {
      blobs.add(b.clone());
    }
    res.blobs = blobs;

    ArrayList<Score> bestScores = new ArrayList<Score>();
    for(Score s: this.bestScores) {
      bestScores.add(s);
    }
    res.bestScores = bestScores;

    ArrayList<Score> leaderboard = new ArrayList<Score>();
    for(Score s: this.leaderboard) {
      leaderboard.add(s);
    }
    res.leaderboard = leaderboard;

    res.beginTime = this.beginTime;
    res.currentTime = this.currentTime;

    return res;
  }

  synchronized void set(Estado e) {
    this.player = new Player(e.player);

    this.enemies = new ArrayList<Player>();
    for(Player p: e.enemies) {
      this.enemies.add(p.clone());
    }
    
    this.blobs = new ArrayList<Blob>();
    for(Blob b: e.blobs) {
      this.blobs.add(b.clone());
    }

    this.bestScores = new ArrayList<Score>();
    for(Score s: e.bestScores) {
      this.bestScores.add(s);
    }

    this.leaderboard = new ArrayList<Score>();
    for(Score s: e.leaderboard) {
      this.leaderboard.add(s);
    }

    this.beginTime = e.beginTime;
    this.currentTime = e.currentTime;
  }
}

class Score {
  String username;
  float score;

  Score(String username, float score) {
    this.username = username;
    this.score = score;
  }
}