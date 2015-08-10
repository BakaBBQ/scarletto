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
import com.badlogic.gdx.graphics.g2d.freetype.FreeTypeFontGenerator.FreeTypeBitmapFontData;

public class PrescreenScreen implements Screen {
  OrthographicCamera camera;
  SpriteBatch batch;
  SpriteBatch uiBatch;
  BitmapFont font;
  FreeTypeFontGenerator fontGen;
  AssetManager manager;
  String currentTextureName;
  int timer = 0;
  String[] texts = {
    "喵喵喵喵喵喵喵喵"
  };

  Hashtable<Integer, String> script = new Hashtable<Integer, String>();
  int currentTextIndex = 0;
  public PrescreenScreen() {
    manager = new AssetManager();
    camera = new OrthographicCamera();
    camera.setToOrtho(false, 960, 720);

    batch = new SpriteBatch();
    uiBatch = new SpriteBatch();
    font = genFont();

    script.put(0, "prescreen/flash1.png");
    script.put(120, "prescreen/flash2.png");
    script.put(240, "prescreen/flash3.png");


    for(int i = 1; i < 5; i++){
      manager.load("prescreen/0" + i + ".jpg", Texture.class);
    }
    for(int i = 6; i < 10; i++){
      manager.load("prescreen/0" + i + ".png", Texture.class);
    }
    manager.load("prescreen/flash1.png", Texture.class);
    manager.load("prescreen/flash2.png", Texture.class);
    manager.load("prescreen/flash3.png", Texture.class);
    manager.finishLoading();
  }

  void nextText(){
    currentTextIndex ++ ;
  }

  String currentText(){
    return texts[currentTextIndex];
  }

  BitmapFont genFont(){
    fontGen = new FreeTypeFontGenerator(Gdx.files.internal("yahei.ttf"));
    FreeTypeFontGenerator.setMaxTextureSize(128);
    FreeTypeFontGenerator.FreeTypeFontParameter fontPar = new FreeTypeFontGenerator.FreeTypeFontParameter();
    fontPar.size = 30;

    StringBuilder builder = new StringBuilder();
    for(String s : texts) {
      builder.append(s);
    }

    fontPar.characters = builder.toString();

    FreeTypeBitmapFontData data = new FreeTypeBitmapFontData() {
			public int getWrapIndex (Array<Glyph> glyphs, int start) {
				return SimplifiedChinese.getWrapIndex(glyphs, start);
			}
		};

    data.xChars = new char[] {'动'};
		data.capChars = new char[] {'动'};

    return fontGen.generateFont(fontPar, data);
  }

  @Override
  public void render(float delta) {
    Gdx.gl.glClearColor(0,0,0,1);
    Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT);



    if(script.containsKey(timer)){
      currentTextureName = script.get(timer);
    }
    timer++;

    camera.update();
    batch.setProjectionMatrix(camera.combined);
    batch.begin();
    font.draw(batch, currentText(), 0, 50, Gdx.graphics.getWidth(), Align.center, false);
    Texture tex = manager.get(currentTextureName, Texture.class);
    batch.draw(tex,0,0);
    batch.end();

    uiBatch.begin();
    uiBatch.end();
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

  static public class SimplifiedChinese {
		public static int getWrapIndex (Array<Glyph> glyphs, int start) {
			for (int i = start; i > 0; i--) {
				int startChar = glyphs.get(i).id;
				if (!SimplifiedChinese.legalAtStart(startChar)) continue;
				int endChar = glyphs.get(i - 1).id;
				if (!SimplifiedChinese.legalAtEnd(endChar)) continue;
				if (startChar < 127 && endChar < 127) continue; // Don't wrap between ASCII chars.
				return i;
			}
			return start;
		}

		static private boolean legalAtStart (int ch) {
			switch (ch) {
			case '!':
			case '%':
			case ')':
			case ',':
			case '.':
			case ':':
			case ';':
			case '>':
			case '?':
			case ']':
			case '}':
			case '¢':
			case '¨':
			case '°':
			case '·':
			case 'ˇ':
			case 'ˉ':
			case '―':
			case '‖':
			case '’':
			case '”':
			case '„':
			case '‟':
			case '†':
			case '‡':
			case '›':
			case '℃':
			case '∶':
			case '、':
			case '。':
			case '〃':
			case '〆':
			case '〈':
			case '《':
			case '「':
			case '『':
			case '〕':
			case '〗':
			case '〞':
			case '﹘':
			case '﹚':
			case '﹜':
			case '！':
			case '＂':
			case '％':
			case '＇':
			case '）':
			case '，':
			case '．':
			case '：':
			case '；':
			case '？':
			case '］':
			case '｀':
			case '｜':
			case '｝':
			case '～':
				return false;
			}
			return true;
		}

		static private boolean legalAtEnd (int ch) {
			switch (ch) {
			case '$':
			case '(':
			case '*':
			case ',':
			case '£':
			case '¥':
			case '·':
			case '‘':
			case '“':
			case '〈':
			case '《':
			case '「':
			case '『':
			case '【':
			case '〔':
			case '〖':
			case '〝':
			case '﹗':
			case '﹙':
			case '﹛':
			case '＄':
			case '（':
			case '．':
			case '［':
			case '｛':
			case '￡':
			case '￥':
				return false;
			}
			return true;
		}
	}
}
