package org.ninesyllables.scarletto;

import com.badlogic.gdx.Gdx ;
import com.badlogic.gdx.Screen ;
import com.badlogic.gdx.graphics.GL20 ;
import com.badlogic.gdx.graphics.OrthographicCamera ;
import com.badlogic.gdx.graphics.g2d.freetype.FreeTypeFontGenerator;
import com.badlogic.gdx.graphics.Texture;
import java.util.Hashtable;
import com.badlogic.gdx.graphics.g2d.BitmapFont;
import com.badlogic.gdx.assets.AssetManager;

import com.badlogic.gdx.utils.Array;
import com.badlogic.gdx.utils.Align;
import com.badlogic.gdx.graphics.g2d.BitmapFont.Glyph;
import com.badlogic.gdx.graphics.g2d.SpriteBatch;
import com.badlogic.gdx.Input.Keys;
import com.badlogic.gdx.graphics.g2d.freetype.FreeTypeFontGenerator.FreeTypeBitmapFontData;


public class PauseScreen implements Screen {
  SpriteBatch batch;

  Texture displayTexture;
  Texture resumeChoice;
  Texture titleChoice;
  Texture retryChoice;

  Texture arrowLeft;
  Texture arrowRight;

  int timer;

  int inputCoolDown;

  int currentIndex;

  int[] indexTimer = {0,0,0};

  public PauseScreen(){
    batch = new SpriteBatch();
    displayTexture = new Texture("pause/menu-indication.png");
    arrowLeft = new Texture("pause/arrow-left.png");
    arrowRight = new Texture("pause/arrow-right.png");
    resumeChoice = new Texture("pause/choice1.png");
    titleChoice = new Texture("pause/choice2.png");
    retryChoice = new Texture("pause/choice3.png");
  }
  @Override
  public void render(float delta) {
    Gdx.gl.glClearColor(0,0,0,1);
    Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT);
    timer ++;

    batch.begin();
    batch.setColor(1,1,1,getOpacity());
    drawOnRelative(displayTexture,0,0);
    setOpacityValue(1);
    if (currentIndex > 0){
      drawOnRelative(arrowLeft, getArrowLeftRelativePos(), 0);
    }
    setOpacityValue(getOpacityValueAccordingToTimer(indexTimer[0])* getOpacity());
    drawOnRelative(resumeChoice,0,0);
    setOpacityValue(getOpacityValueAccordingToTimer(indexTimer[1])* getOpacity());
    drawOnRelative(titleChoice,0,0);
    setOpacityValue(getOpacityValueAccordingToTimer(indexTimer[2])* getOpacity());
    drawOnRelative(retryChoice,0,0);
    setOpacityValue(1 * getOpacity());
    if (currentIndex < 2){
      drawOnRelative(arrowRight, getArrowRightRelativePos(), 0);
    }
    batch.setColor(1,1,1,1);
    batch.end();

    if (Gdx.input.isKeyPressed(Keys.LEFT) && currentIndex > 0 && inputCoolDown <= 0){
      currentIndex --;
      inputCoolDown = 20;
    }



    if (Gdx.input.isKeyPressed(Keys.RIGHT) && currentIndex < 2 && inputCoolDown <= 0){
      currentIndex ++;
      inputCoolDown = 20;
    }
    indexTimer[currentIndex]+=2;
    for(int i = 0; i < 3; i ++){
      indexTimer[i]--;
      if (indexTimer[i] < 0)
        indexTimer[i] = 0;
      if (indexTimer[i] > 20)
        indexTimer[i] = 20;
    }

    inputCoolDown--;
  }

  int getArrowLeftRelativePos(){
    int timerQuot = timer / 20;
    int offSet = timerQuot % 7;
    return Math.abs(-3 + offSet);
  }

  void setOpacityValue(float opacity){
    batch.setColor(1,1,1,opacity);
  }

  float getOpacityValueAccordingToTimer(int timer){
    return Math.min(timer * 1, 20)/20f;
  }

  int getArrowRightRelativePos(){
    return getArrowLeftRelativePos() * -1;
  }

  float getOpacity(){
    return Math.min(1*timer, 60)/60f;
  }

  void drawOnRelative(Texture texture, int x, int y){
    int offSetX = 351;
    int offSetY = 274;
    int finalX = x + offSetX;
    int finalY = y + offSetY;
    batch.draw(texture, finalX, finalY);
  }

  @Override
  public void resize(int width, int height) {
  }

  @Override
  public void show() {
  }

  @Override
  public void hide() {
  }

  @Override
  public void pause() {
  }

  @Override
  public void resume() {
  }

  @Override
  public void dispose() {
  }
}
