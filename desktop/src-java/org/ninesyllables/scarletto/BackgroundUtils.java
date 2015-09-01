package org.ninesyllables.scarletto;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.graphics.*;
import com.badlogic.gdx.graphics.g3d.*;
import com.badlogic.gdx.graphics.g3d.attributes.ColorAttribute;
import com.badlogic.gdx.graphics.g3d.attributes.TextureAttribute;
import com.badlogic.gdx.graphics.g3d.environment.DirectionalLight;
import com.badlogic.gdx.graphics.g3d.utils.CameraInputController;
import com.badlogic.gdx.graphics.g3d.utils.MeshPartBuilder;
import com.badlogic.gdx.graphics.g3d.utils.ModelBuilder;
import scarletto.prologue.Background;

/**
 * Created by LBQ on 2015/9/1.
 */
public class BackgroundUtils {
    PerspectiveCamera perspectiveCamera;
    Environment environment;
    ModelBatch modelBatch;

    Model model1;
    Model model2;
    Model model3;

    public Model model;
    public ModelInstance instance;


    ModelInstance i1;
    public BackgroundUtils(int gameWidth, int gameHeight){
        modelBatch = new ModelBatch();


        perspectiveCamera = new PerspectiveCamera(67, gameWidth, gameHeight);
        //perspectiveCamera.translate(512, 50, 256);
        //perspectiveCamera.lookAt(768, 60, 256);
        perspectiveCamera.position.set(40,40,40);
        perspectiveCamera.rotate(-0.1f, 0, 1, 0);
        perspectiveCamera.near = 1;
        perspectiveCamera.far = 300;
        perspectiveCamera.rotate(-78, 0, 0, 1);
        perspectiveCamera.lookAt(0, 0, 0);
        perspectiveCamera.update();

        ModelBuilder modelBuilder = new ModelBuilder();
        model = modelBuilder.createBox(30f, 30f, 30f,
                new Material(new TextureAttribute(TextureAttribute.Diffuse, new Texture("side.png"))),
                VertexAttributes.Usage.Position | VertexAttributes.Usage.Normal | VertexAttributes.Usage.TextureCoordinates);
        instance = new ModelInstance(model);

        environment = new Environment();
        environment.set(new ColorAttribute(ColorAttribute.AmbientLight, 0.4f, 0.4f, 0.4f, 1f));
        environment.add(new DirectionalLight().set(0.8f, 0.8f, 0.8f, -1f, -0.8f, -0.2f));

        model1 = createPlaneModel(512, 512, new Material(new TextureAttribute(TextureAttribute.Diffuse, new Texture("side.png"))),
                512,512,512,512);
        i1 = new ModelInstance(model1);

    }

    // http://stackoverflow.com/questions/21749483/libgdx-rendering-floor-for-3d-game
    private Model createPlaneModel(final float width, final float height, final Material material,
                                   final float u1, final float v1, final float u2, final float v2) {
        ModelBuilder modelBuilder = new ModelBuilder();
        modelBuilder.begin();
        MeshPartBuilder bPartBuilder = modelBuilder.part("rect",
                GL20.GL_TRIANGLES, VertexAttributes.Usage.Position | VertexAttributes.Usage.Normal | VertexAttributes.Usage.TextureCoordinates,
                material);
//NOTE ON TEXTURE REGION, MAY FILL OTHER REGIONS, USE GET region.getU() and so on
        bPartBuilder.setUVRange(u1, v1, u2, v2);
        bPartBuilder.rect(
                -(width * 0.5f), -(height * 0.5f), 0,
                (width * 0.5f), -(height * 0.5f), 0,
                (width * 0.5f), (height * 0.5f), 0,
                -(width * 0.5f), (height * 0.5f), 0,
                0, 0, -1);

        return (modelBuilder.end());
    }

    public void render(){

        //perspectiveCamera.rotate(1,1,1,1);
        modelBatch.begin(perspectiveCamera);
        //modelBatch.render(i1, environment);
        modelBatch.render(instance, environment);
        modelBatch.end();
    }
}
