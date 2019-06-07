class Reader extends Thread {
  BufferedReader b;
  String username;
  int iconIdx;
  final float scaleX = width/(float)1366;
  final float scaleY = height/(float)768;

  Reader(BufferedReader in, String username, int iconIdx) {
    this.b = in;
    this.username = username;
    this.iconIdx = iconIdx;
  }

  public void run() {
    while (true) {
      try {
        String info = this.b.readLine();
        if (info.equals("MatchOverBegin")) {
          ArrayList<Score> leaderboard = new ArrayList<Score>();
          info = this.b.readLine();
          while (!info.equals("MatchOverEnd")) {
            String[] fields = info.split(":");
            leaderboard.add(new Score(fields[1], Float.parseFloat(fields[2])));
            info = this.b.readLine();
          }
          Estado e = estado.get();
          e.leaderboard = leaderboard;
          estado.set(e);
          matchMenu.set(false);
          leaderboardMenu.set(true);
          if(estado.player.username.equals(estado.leaderboard.get(0).username))
            switchMusic(3);
          else
            switchMusic(4);
          break;
          
        } else if (info.equals("StartInitialMatchInfo")) {
          Estado e = new Estado();
          e.enemies = new ArrayList<Player>();
          e.blobs = new ArrayList<Blob>();
          e.bestScores = new ArrayList<Score>();
          e.leaderboard = new ArrayList<Score>();
          info = this.b.readLine();
          while (!info.equals("EndInitialMatchInfo")) {
            String[] fields = info.split(":");
            if (fields[0].equals("P")) {
              if (fields[1].equals(this.username))
                // Utilizador da interface
                e.player = new Player(this.username, Float.parseFloat(fields[2])*scaleX, Float.parseFloat(fields[3])*scaleY, Float.parseFloat(fields[4])*scaleX, Float.parseFloat(fields[5]), false, this.iconIdx, Boolean.parseBoolean(fields[6]), Boolean.parseBoolean(fields[7]));
              else
                // Adversários
                e.enemies.add(new Player(fields[1], Float.parseFloat(fields[2])*scaleX, Float.parseFloat(fields[3])*scaleY, Float.parseFloat(fields[4])*scaleX, Float.parseFloat(fields[5]), true, 0, Boolean.parseBoolean(fields[6]), Boolean.parseBoolean(fields[7])));
            } else if (fields[0].equals("B")) {
              if (fields[2].equals("poison"))
                e.blobs.add(new Blob(Float.parseFloat(fields[3]) *scaleX, Float.parseFloat(fields[4]) *scaleY, Float.parseFloat(fields[5])*scaleX, 1));
              else if (fields[2].equals("food"))
                e.blobs.add(new Blob(Float.parseFloat(fields[3]) *scaleX, Float.parseFloat(fields[4]) *scaleY, Float.parseFloat(fields[5])*scaleX, 0));
              else if (fields[2].equals("invisibilityBonus"))
                e.blobs.add(new Blob(Float.parseFloat(fields[3]) *scaleX, Float.parseFloat(fields[4]) *scaleY, Float.parseFloat(fields[5])*scaleX, 2));
              else if (fields[2].equals("speedBonus"))
                e.blobs.add(new Blob(Float.parseFloat(fields[3]) *scaleX, Float.parseFloat(fields[4]) *scaleY, Float.parseFloat(fields[5])*scaleX, 3));
            } else if (fields[0].equals("CT")) {
              e.beginTime = Integer.parseInt(fields[1]);
              e.currentTime = Integer.parseInt(fields[1]);
            } else if (fields[0].equals("S")) {
              e.bestScores.add(new Score(fields[1], Float.parseFloat(fields[2])));
            }
            info = this.b.readLine();
          }
          estado.set(e);
          waitMatch.set(false);
          matchMenu.set(true);
          switchMusic(2);
        } else if (info.equals("StartMatchInfo")) {
          Estado e = estado.get();
          ArrayList<Score> scores = new ArrayList<Score>();
          info = this.b.readLine();
          while (!info.equals("EndMatchInfo")) {
            String[] fields = info.split(":");
            if (fields[0].equals("P")) {
              if (fields[1].equals(this.username)) {
                // Utilizador da interface
                e.player.username = this.username;
                e.player.move(Float.parseFloat(fields[2]) *scaleX, Float.parseFloat(fields[3]) * scaleY);
                e.player.r = Float.parseFloat(fields[4])*scaleX;
                e.player.score = Float.parseFloat(fields[5]);
                e.player.invisibilityBonus =  Boolean.parseBoolean(fields[6]);
                e.player.speedBonus = Boolean.parseBoolean(fields[7]);
              } else {
                // Adversários
                Player p = null;
                String username = fields[1];
                float x = Float.parseFloat(fields[2])*scaleX;
                float y = Float.parseFloat(fields[3])*scaleY;
                float r = Float.parseFloat(fields[4])*scaleX;
                float score = Float.parseFloat(fields[5]);
                boolean invisibilityBonus = Boolean.parseBoolean(fields[6]);
                boolean speedBonus =  Boolean.parseBoolean(fields[7]);
                for (int i = 0; i < e.enemies.size(); i++) {
                  p = e.enemies.get(i);
                  if (username.equals(p.username))
                    break;
                }
                p.move(x, y);
                p.r = r;
                p.score = score;
                p.invisibilityBonus = invisibilityBonus;
                p.speedBonus = speedBonus;
              }
            } else if (fields[0].equals("B")) {
              int type = -1;
              if (fields[2].equals("poison"))
                type = 1;
              else if (fields[2].equals("food"))
                type = 0;
              else if (fields[2].equals("invisibilityBonus"))
                type = 2;
              else if (fields[2].equals("speedBonus"))
                type = 3;
              float x = Float.parseFloat(fields[3]) *scaleX;
              float y = Float.parseFloat(fields[4]) *scaleY;
              float r = Float.parseFloat(fields[5]) *scaleX;
              e.blobs.set(Integer.parseInt(fields[1]), new Blob(x, y, r, type));
            } else if (fields[0].equals("CT")) {
              e.currentTime = Integer.parseInt(fields[1]);
            } else if (fields[0].equals("S")) {
              scores.add(new Score(fields[1], Float.parseFloat(fields[2])));
            }
            info = this.b.readLine();
          }
          if (scores.size() != 0)
            e.bestScores = scores;
          estado.set(e);
        }
      } catch (Exception e) {
        e.printStackTrace();
        break;
      }
    }
  }
}
