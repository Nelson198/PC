final float minMass = 16*16*PI;

class Blob {
  PVector pos;
  float r;
  int type; //0 -> comestivel; 1 -> venenosa; 2 -> invisibilityBonus, 3 -> speedBonus

  Blob() {
    this.pos = new PVector(0,0);
    this.r = 0;
    this.type = 0;
  }

  Blob(float _x, float _y, float _r, int _type) {
    this.pos = new PVector(_x, _y);
    this.r = _r;
    this.type = _type;
  }

  Blob(Blob otherBlob) {
    this.pos = otherBlob.getPosition();
    this.r = otherBlob.getRadius();
    this.type = otherBlob.getType();
  }

  PVector getPosition() {
    return this.pos.copy();
  }

  float getRadius() {
    return this.r;
  }

  int getType() {
    return this.type;
  }

  void show() {
    if (this.type == 0) {
      fill(0, 255, 0);
      ellipse(this.pos.x, this.pos.y, r*2, r*2);
    } else if (this.type == 1) {
      fill(255, 0, 0);
      ellipse(this.pos.x, this.pos.y, r*2, r*2);
    } else if (this.type == 2){
      fill(100, 100, 0);
      square(this.pos.x, this.pos.y, this.r*2);
    } else if (this.type == 3) {
      fill(0, 100, 100);
      square(this.pos.x, this.pos.y, this.r*2);
    } 
    strokeWeight(1);
    stroke(0);
  }

  Blob clone() {
    return new Blob(this);
  }
}
