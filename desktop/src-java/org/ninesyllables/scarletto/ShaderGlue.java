package org.ninesyllables.scarletto;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.graphics.g2d.SpriteBatch;
import com.badlogic.gdx.graphics.glutils.ShaderProgram;
import com.badlogic.gdx.utils.GdxRuntimeException;

/**
 * Created by LBQ on 2015/9/1.
 */
public class ShaderGlue {
    public static ShaderProgram genShaderProgram() {
        String vertexShader = Gdx.files.internal("shaders/vertex.glsl").readString();
        String fragmentShader = Gdx.files.internal("shaders/fragment.glsl").readString();
        ShaderProgram p = new ShaderProgram(SpriteBatch.createDefaultShader().getVertexShaderSource(), fragmentShader);
        if (!p.isCompiled()) throw new GdxRuntimeException("Couldn't compile shader: " + p.getLog());
        return p;
    }

}
