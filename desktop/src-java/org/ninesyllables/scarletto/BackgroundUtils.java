package org.ninesyllables.scarletto;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.Input;
import com.badlogic.gdx.graphics.*;
import com.badlogic.gdx.graphics.Color;
import com.badlogic.gdx.graphics.g3d.*;
import com.badlogic.gdx.graphics.g3d.attributes.ColorAttribute;
import com.badlogic.gdx.graphics.g3d.attributes.PointLightsAttribute;
import com.badlogic.gdx.graphics.g3d.attributes.TextureAttribute;
import com.badlogic.gdx.graphics.g3d.environment.DirectionalLight;
import com.badlogic.gdx.graphics.g3d.environment.PointLight;
import com.badlogic.gdx.graphics.g3d.environment.SpotLight;
import com.badlogic.gdx.graphics.g3d.utils.CameraInputController;
import com.badlogic.gdx.graphics.g3d.utils.MeshPartBuilder;
import com.badlogic.gdx.graphics.g3d.utils.ModelBuilder;
import com.badlogic.gdx.math.Vector3;
import com.badlogic.gdx.utils.Array;
import scarletto.prologue.Background;

import java.awt.*;
import java.util.Random;

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
    public Array<ModelInstance> sceneInstances;

    PointLight pointLight;

    Array<PointLight> pointLights;


    Vector3 cameraInitPos = new Vector3(10, 15, 0);

    int timer = 0;
    float speedRate = 0.1f;

    ModelInstance i1;
    public BackgroundUtils(int gameWidth, int gameHeight){
        pointLights = new Array<>();
        modelBatch = new ModelBatch();
        sceneInstances = new Array<ModelInstance>();

        perspectiveCamera = new PerspectiveCamera(67, gameWidth, gameHeight);
        //perspectiveCamera.translate(512, 50, 256);
        //perspectiveCamera.lookAt(768, 60, 256);
        perspectiveCamera.position.set(cameraInitPos);
        //perspectiveCamera.rotate(-0.1f, 0, 1, 0);
        perspectiveCamera.near = 1;
        perspectiveCamera.far = 300;
        perspectiveCamera.rotate(-90, 0, 0, 1);
        perspectiveCamera.lookAt(1, 15, 10);
        perspectiveCamera.update();

        ModelBuilder modelBuilder = new ModelBuilder();
        model = modelBuilder.createBox(30f, 1f, 30f,
                new Material(new TextureAttribute(TextureAttribute.Diffuse, new Texture("side.png"))),
                VertexAttributes.Usage.Position | VertexAttributes.Usage.Normal | VertexAttributes.Usage.TextureCoordinates);
        Model modelFloor = modelBuilder.createBox(1f, 30f, 30f,
                new Material(new TextureAttribute(TextureAttribute.Diffuse, new Texture("floor.png"))),
                VertexAttributes.Usage.Position | VertexAttributes.Usage.Normal | VertexAttributes.Usage.TextureCoordinates);
        instance = new ModelInstance(model);

        environment = new Environment();
        environment.set(new ColorAttribute(ColorAttribute.AmbientLight, 0.1f, 0.1f, 0.1f, 100f));
        //environment.add(new DirectionalLight().set(0.4f, 0.4f, 0.8f, -1f, 0f, 0f));

        final float luminousity = 1f;
        pointLights.add(new PointLight().set(Color.BLUE,new Vector3(24,36,16), luminousity));
        pointLights.add(new PointLight().set(Color.BLUE,new Vector3(8,20,9), luminousity));
        pointLights.add(new PointLight().set(Color.BLUE, new Vector3(8, 20, 16), luminousity));
        pointLights.add(new PointLight().set(Color.BLUE,new Vector3(22,36,9), luminousity));
        for (PointLight pl : pointLights){
            environment.add(pl);
        }



//        environment.add(new PointLight().set(Color.BLUE,new Vector3(22,36,9 + 90), 40f));
//        environment.add(new PointLight().set(Color.BLUE,new Vector3(24,36,16 + 90), 40f));
//        environment.add(new PointLight().set(Color.BLUE,new Vector3(8,20,9 + 90), 40f));
//        environment.add(new PointLight().set(Color.BLUE,new Vector3(8,20,16 + 90), 40f));
//        environment.add(new PointLight().set(Color.BLUE,new Vector3(16,12,16), 100f));

        for (int i = 0; i < 10; i ++){
            ModelInstance newOne = new ModelInstance(model);
            newOne.transform.setToTranslation(0, 0, i * 30);

            ModelInstance newOne2 = new ModelInstance(modelFloor);
            newOne2.transform.setToTranslation(-15, 15, i * 30);

            ModelInstance newOne3 = new ModelInstance(model);
            newOne3.transform.setToTranslation(0, 30, i * 30);

            sceneInstances.add(newOne);
            sceneInstances.add(newOne2);
            sceneInstances.add(newOne3);
        }

    }

    public void render(){
        timer++;
        perspectiveCamera.position.set(new Vector3(cameraInitPos.x, cameraInitPos.y, cameraInitPos.z + (timer * speedRate) % 60));
        perspectiveCamera.update();
        for (PointLight pl : pointLights){
//            pl.setPosition(pl.position.x, pl.position.y, pl.position.z + (timer * speedRate) % 120);
            environment.add(pl);

        }
//        for (Attribute a : environment){
//            if (a instanceof PointLightsAttribute){
//                for (PointLight pl : ((PointLightsAttribute) a).lights){
//                    pl.position.set(pl.position.x, pl.position.y, pl.position.z + (timer * speedRate) % 120);
//                }
//            }
//        }



        //perspectiveCamera.rotate(1,1,1,1);
        modelBatch.begin(perspectiveCamera);
        //modelBatch.render(i1, environment);
        for (ModelInstance i : sceneInstances){
            modelBatch.render(i, environment);
        }
        //modelBatch.render(instance, environment);
        modelBatch.end();
        for (PointLight pl : pointLights){
            environment.remove(pl);
        }
    }
}
