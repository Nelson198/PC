import java.lang.Comparable;

class Player extends Blob implements Comparable{
  String username;
  boolean isEnemy;
  float score;
  int iconIdx;
  boolean invisibilityBonus;
  boolean speedBonus;

  Player() {
    super();
    this.username = "";
    this.isEnemy = false;
    this.score = 0;
    this.iconIdx = 5;
  }

  Player(String username, float _x, float _y, float _r, float score, boolean isEnemy, int iconIdx, boolean invisibilityBonus, boolean speedBonus) {
    super(_x, _y, _r, -1);
    this.username = username;
    this.score = score;
    this.isEnemy = isEnemy;
    this.iconIdx = iconIdx;
    this.invisibilityBonus = invisibilityBonus;
    this.speedBonus = speedBonus;
  }

  Player(Player otherPlayer) {
    super(otherPlayer.getPosition().x , otherPlayer.getPosition().y, otherPlayer.getRadius(), 2);
    this.username = otherPlayer.getUsername();
    this.isEnemy = otherPlayer.getIsEnemy();
    this.score = otherPlayer.score;
    this.iconIdx = otherPlayer.iconIdx;
    this.invisibilityBonus = otherPlayer.invisibilityBonus;
    this.speedBonus = otherPlayer.speedBonus;
  }

  synchronized int getIconIdx() {
    return this.iconIdx;
  }

  synchronized void setIconIdx(int iconIdx) {
    this.iconIdx = iconIdx;
  }

  String getUsername() {
    return this.username;
  }

  boolean getIsEnemy() {
    return this.isEnemy;
  }

  void move(float x, float y) {
    this.pos = new PVector(x, y);
  }

  int compareTo(Object object) {
    if(object == null)
      return 0;
    Player p = (Player)object;
    if (this.r > p.r)
      return 1;
    else
      return -1;
  }

  void showIcons() {
    PShape sh = icons.get(this.iconIdx);
    if (this.invisibilityBonus && this.speedBonus)
      fill(100, 120);
    else if (this.speedBonus)
      fill(0, 100, 100, 150);
    else if (this.invisibilityBonus)
      fill(255, 120);
    else
      fill(255, 0);

    shape(sh, pos.x, pos.y, r*2, r*2);
    ellipse(pos.x, pos.y, r*2, r*2);

    textSize(this.r/3);
    if (this.isEnemy)
      fill(255, 0, 0);
    else
      fill(0, 0, 255);
    textAlign(CENTER, CENTER);
    text(this.score, pos.x, pos.y);
  }

  void showNormal() {
    if (this.invisibilityBonus && this.speedBonus)
      fill(128, 128, 128, 120);
    else if (this.speedBonus)
        fill(128);
    else if (this.invisibilityBonus)
      fill(255, 255, 255, 120);
    else
      fill(0);
    ellipse(pos.x, pos.y, r*2, r*2);
    
    // Borda ondulada
    float smoothness = 0.1; // Quanto menor é este valor, mais suave é a borda
    int maxOffset = 4; // Número de píxeis máximo que a borda se vai afastar da circunferência
    noFill();
    beginShape();
    for(int i = 0; i < 360; i++){
      float x = this.pos.x + this.r*cos(TWO_PI*i/360);
      float y = this.pos.y + this.r*sin(TWO_PI*i/360);
      float n = map(noise(smoothness*(frameCount+x), smoothness*(frameCount+y), 0), 0, 1, -maxOffset, maxOffset);
      x += n;
      y += n;
      vertex(x, y);
    }
    endShape();

    textSize(this.r/3);
    if(this.invisibilityBonus)
      fill(0);
    else
      fill(255);
    textAlign(CENTER, CENTER);
    text(this.score, pos.x, pos.y);
  }

  void show() {
    if (this.isEnemy && this.invisibilityBonus)
      return;

    strokeWeight(4);
    if (this.isEnemy)
      stroke(255, 0, 0);
    else
      stroke(0, 0, 255);

    if(iconsOn.get())
      showIcons();
    else
      showNormal();

    fill(0);
    if (!this.isEnemy) {
      fill(0, 0, 255);
    }
    textSize(18);
    textAlign(CENTER,TOP);
    text(this.username, pos.x, (pos.y - this.r) - 25);
    strokeWeight(1);
    stroke(0);
    fill(0);
  }

  Player clone() {
    return new Player(this);
  }
}
