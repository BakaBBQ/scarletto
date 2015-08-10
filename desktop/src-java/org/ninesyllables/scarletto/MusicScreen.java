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


public class MusicScreen implements Screen {
  SpriteBatch batch;
  int timer;

  int currentIndex;
  int inputCoolDown;

  int[] indexTimer = {0,0,0,0,0};

  public MusicScreen(){
    batch = new SpriteBatch();
  }

  @Override
  public void render(float delta) {
    Gdx.gl.glClearColor(0,0,0,1);
    Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT);
    timer ++;

    batch.begin();
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
    for(int i = 0; i < 5; i ++){
      indexTimer[i]--;
      if (indexTimer[i] < 0)
        indexTimer[i] = 0;
      if (indexTimer[i] > 20)
        indexTimer[i] = 20;
    }

    inputCoolDown--;
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
